{-# LANGUAGE MultiWayIf, OverloadedStrings, ScopedTypeVariables #-}

module Main(main) where

import System.Directory
import System.Process
import System.IO

import Control.Monad

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashSet as HSet
import qualified Data.Text.Read as T
import qualified Data.Text as T
import Data.Foldable
import Data.Map (Map)
import Data.Functor
import Data.Default
import Data.List
import Data.Char

import qualified Text.HTML.TagSoup as TS

import Text.Pandoc.Highlighting
import Text.Pandoc.Options
import Text.Sass

import Hakyll.Core.Configuration
import Hakyll

import qualified Skylighting as S
import Skylighting (TokenType(..))

compress :: Bool
compress = True

compresses :: Applicative m => (a -> m a) -> a -> m a
compresses f = if compress then f else pure

main :: IO ()
main = hakyllWith def { previewHost = "0.0.0.0"
                      , previewPort = 8080
                      } $ do

  match "assets/*.svg" $ do
    route idRoute
    compile $ getResourceString
          >>= compresses minifyHtml

  match "assets/css/main.scss" $ do
    route $ setExtension "css"
    compile sassCompiler

  -- We load all other files and just compile them to units. This is a little
  -- dubious, but saves us doing anything complex.
  match ("assets/**.scss" .||. "node_modules/**.css") $ compile (makeItem ())

  match ("assets/**.png" .||. "*.ico") $ do
    route   idRoute
    compile copyFileCompiler

  match "index.html" $ do
    route idRoute
    compile $ do
      syntaxMap <- loadAllSnapshots "syntax/*.xml" "syntax"
               <&> foldr (S.addSyntaxDefinition . itemBody) S.defaultSyntaxMap
      getResourceBody
         >>= applyAsTemplate siteCtx
         >>= highlightAmulet syntaxMap
         >>= defaultTemplate

  match "tutorials/*.ml" $ do
    route $ setExtension "html"
    compile $ do
      opts <- writerOptions
      Item _ contents <- getResourceBody

      body <- unsafeCompiler $ do
        (path, handle) <- openTempFile "/tmp/" "example.ml"
        hPutStr handle contents
        hFlush handle
        body <- readProcess "amc-example" [ path ] ""
        removePathForcibly path
        pure body

      let contents = Item (fromFilePath "example.md") body
      Item _ contents <- writePandocWith opts <$> readPandocWith readerOptions contents

      makeItem contents
        >>= loadAndApplyTemplate "templates/example.html" defaultContext
        >>= contentTemplate

  match "tutorials/*.md" $ do
    route $ setExtension "html"
    compile $ pandocCustomCompiler
          >>= loadAndApplyTemplate "templates/tutorial.html" defaultContext
          >>= contentTemplate

  match "tutorials/index.html" $ do
    route idRoute
    compile $ do
      let indexCtx = defaultContext
            <> listField "entries" siteCtx (loadAll "tutorials/*.md")
            <> listField "examples" siteCtx (loadAll "tutorials/*.ml")
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= contentTemplate

  match "reference/*.md" $ do
    route $ setExtension "html"
    compile $ pandocCustomCompiler
          >>= contentTemplate

  match "templates/*" $ compile templateBodyCompiler

  match "syntax/*.xml" $ compile $ do
    path <- toFilePath <$> getUnderlying
    contents <- itemBody <$> getResourceBody
    debugCompiler ("Loaded syntax definition from " ++ show path)
    res <- unsafeCompiler (S.parseSyntaxDefinitionFromString path contents)
    _ <- saveSnapshot "syntax" =<< either fail makeItem res
    makeItem contents


-- | The default context for the whole site, including site-global
-- properties.
siteCtx :: Context String
siteCtx = defaultContext
       <> constField "site.title" "Amulet ML"
       <> constField "site.description" "Amulet is a simple, functional programming language in the ML tradition"

          -- TODO: This needs to be done on the output!
       <> field "site.versions.main_css" (const . hashCompiler . fromFilePath $ "assets/css/main.scss")

-- | Apply the default content template
contentTemplate :: Item String -> Compiler (Item String)
contentTemplate =
      loadAndApplyTemplate "templates/content.html" defaultContext
  >=> defaultTemplate

-- | Apply the default HTML template
defaultTemplate :: Item String -> Compiler (Item String)
defaultTemplate =
      loadAndApplyTemplate "templates/default.html" siteCtx
  >=> compresses minifyHtml

-- | Looks around for blocks marked as @data-language=@ and highlight them.
highlightAmulet :: Map T.Text S.Syntax -> Item String -> Compiler (Item String)
highlightAmulet syntaxMap = pure . fmap (withTagList walk) where
  walk [] = []
  walk (o@(TS.TagOpen "pre" attrs):TS.TagText src:c@(TS.TagClose "pre"):xs)
    | Just (_, language) <- find (\(tag, _) -> tag == "data-language") attrs
    = o : highlight language src ++ c:walk xs
  walk (x:xs) = x:walk xs

  -- | Normalise a highlighted block to trim trailing/leading line and remove the indent.
  dropIndent :: T.Text -> T.Text
  dropIndent t =
    let lines = dropWhile (T.all isSpace) . dropWhileEnd (T.all isSpace) . T.lines $ t
        indent = minimum . map (T.length . T.takeWhile isSpace) . filter (not . T.null) $ lines
    in T.intercalate "\n" . map (T.drop indent) $ lines

  highlight :: String -> String -> [TS.Tag String]
  highlight language txt =
    let Just syntax = S.lookupSyntax (T.pack language) syntaxMap
        Right lines = S.tokenize (S.TokenizerConfig syntaxMap False) syntax . dropIndent . T.pack $ txt
    in foldr (flip (foldr mkElement . (TS.TagText "\n":))) [] lines

  mkElement :: S.Token -> [TS.Tag String] -> [TS.Tag String]
  mkElement (ty, txt) xs
    | CommentTok <- ty
    , (before, txt') <- T.span isCommenty txt
    , (txt'', after) <- T.span isDigit txt'
    , T.all isCommenty after
    , Right (idx :: Int, "") <- T.decimal txt''
      -- If we can strip all punctuation and spaces, and end up with a
      -- number, assume this is an annotation indicator.
    = TS.TagOpen "span" [("class", "annotate")]
    : TS.TagOpen "span" [("class", short CommentTok)] : TS.TagText (T.unpack before) : TS.TagClose "span"
    : TS.TagText (show idx)
    : TS.TagOpen "span" [("class", short CommentTok)] : TS.TagText (T.unpack after) : TS.TagClose "span"
    : TS.TagClose "span"
    : xs

    | NormalTok <- ty
    = TS.TagText (T.unpack txt) : xs

    | otherwise
    = TS.TagOpen "span" [("class", short ty)]
    : TS.TagText (T.unpack txt)
    : TS.TagClose "span"
    : xs

  -- | Is this likely to be part of comment preamble?
  isCommenty x = isPunctuation x || isSpace x

  -- | See Skylighting.Format.HTML
  short :: TokenType -> String
  short KeywordTok        = "kw"
  short DataTypeTok       = "dt"
  short DecValTok         = "dv"
  short BaseNTok          = "bn"
  short FloatTok          = "fl"
  short CharTok           = "ch"
  short StringTok         = "st"
  short CommentTok        = "co"
  short OtherTok          = "ot"
  short AlertTok          = "al"
  short FunctionTok       = "fu"
  short RegionMarkerTok   = "re"
  short ErrorTok          = "er"
  short ConstantTok       = "cn"
  short SpecialCharTok    = "sc"
  short VerbatimStringTok = "vs"
  short SpecialStringTok  = "ss"
  short ImportTok         = "im"
  short DocumentationTok  = "do"
  short AnnotationTok     = "an"
  short CommentVarTok     = "cv"
  short VariableTok       = "va"
  short ControlFlowTok    = "cf"
  short OperatorTok       = "op"
  short BuiltInTok        = "bu"
  short ExtensionTok      = "ex"
  short PreprocessorTok   = "pp"
  short AttributeTok      = "at"
  short InformationTok    = "in"
  short WarningTok        = "wa"
  short NormalTok         = ""

-- | Attempts to minify the HTML contents by removing all superfluous
-- whitespace.
minifyHtml :: Item String -> Compiler (Item String)
minifyHtml = pure . fmap minifyHtml'

-- | The main worker for minifyHtml.
minifyHtml' :: String -> String
minifyHtml' = withTagList (walk [] [] []) where
  walk _ _ _ [] = []
  walk noTrims noCollapses inlines (x:xs) = case x of
    o@(TS.TagOpen tag _) ->
      o:walk (maybeCons (noTrim tag) tag noTrims)
             (maybeCons (noCollapse tag) tag noCollapses)
             (maybeCons (inline tag) tag inlines)
             xs

    TS.TagText text -> (:walk noTrims noCollapses inlines xs) . TS.TagText $
      if
        | null noCollapses -> collapse (null inlines) text
        | null noTrims     -> trim (null inlines) text
        | otherwise        -> text

    c@(TS.TagClose tag) ->
      c:walk (maybeDrop tag noTrims)
             (maybeDrop tag noCollapses)
             (maybeDrop tag inlines)
             xs

    -- Strip metadata
    TS.TagComment{}  -> walk noTrims noCollapses inlines xs
    TS.TagWarning{}  -> walk noTrims noCollapses inlines xs
    TS.TagPosition{} -> walk noTrims noCollapses inlines xs

  noTrim, noCollapse, inline :: String -> Bool
  -- | Tags which should not have whitespace touched (consecutive spaces
  -- merged, or leading/trailing spaces trimmed).
  noCollapse = flip HSet.member $ HSet.fromList
    [ "pre", "textarea", "script", "style" ]
  -- | Tags which should not have whitespace trimmed.
  noTrim = flip HSet.member $ HSet.fromList
    [ "pre", "textarea" ]
  -- | Tags which are "inline" or contain inline content, and thus should
  -- have leading/trailing spaces preserved.
  inline = flip HSet.member $ HSet.fromList
    [ "a", "abbr", "acronym", "b", "bdi", "bdo", "big", "button", "cite", "code"
    , "del", "dfn", "em", "font", "figcaption", "i", "img", "input", "ins", "kbd"
    , "label" , "li", "mark", "math", "nobr", "object", "p", "q", "rp", "rt"
    , "rtc", "ruby", "s", "samp", "select", "small", "span", "strike", "strong"
    , "sub", "sup", "svg", "td", "textarea", "time", "tt", "u", "var", "wbr"
    ]

  trim _ "" = ""
  trim strip xs =
    let isLast = not strip && isSpace (last xs)
        isFirst = not strip && isSpace (head xs)

        space True = " "
        space False = ""
    in
    case dropWhile isSpace . dropWhileEnd isSpace $ xs of
      "" -> space (isFirst || isLast)
      xs -> space isFirst ++ xs ++ space isLast

  -- | Collapse adjacent spaces into one, and optionally trim the front/back
  collapse strip = trim strip . collapse'

  -- | Collapses adjacent spaces into one
  collapse' [] = []
  collapse' (x:xs)
    | isSpace x = ' ':collapse' (dropWhile isSpace xs)
    | otherwise = x:collapse' xs

  maybeDrop y (x:xs) | x == y = xs
  maybeDrop _ xs = xs

  maybeCons True x xs = x:xs
  maybeCons False _ xs = xs

-- | Generate a trivial cachebuster hash of a cached identifier.
--
-- Note that this should be a file in the main directory, not result of a
-- match.
hashCompiler :: Identifier -> Compiler String
hashCompiler x = take 16 . SHA.showDigest . SHA.sha256 . BS.pack <$> loadBody x

-- | The Pandoc compiler, but using our custom 'writerOptions'.
pandocCustomCompiler :: Compiler (Item String)
pandocCustomCompiler = pandocCompilerWith readerOptions =<< writerOptions

readerOptions :: ReaderOptions
readerOptions = def
  { readerExtensions = pandocExtensions
  , readerIndentedCodeClasses = ["amulet"] }

writerOptions :: Compiler WriterOptions
writerOptions = do
  syntaxMap <- loadAllSnapshots "syntax/*.xml" "syntax"
           <&> foldr (S.addSyntaxDefinition . itemBody) S.defaultSyntaxMap

  pure $ defaultHakyllWriterOptions
    { writerExtensions = extensionsFromList
                         [ Ext_tex_math_dollars
                         , Ext_tex_math_double_backslash
                         , Ext_latex_macros
                         ] <> writerExtensions defaultHakyllWriterOptions
    , writerHTMLMathMethod = MathJax ""
    , writerSyntaxMap = syntaxMap
    , writerHighlightStyle = Just kate
    }

-- | Compiles sass files to CSS.
--
-- Similar to that provided by hakyll-sass, but correctly handles
-- dependencies.
sassCompiler :: Compiler (Item String)
sassCompiler = do
  path <- getResourceFilePath
  (contents, deps) <- unsafeCompiler $ do
    result <- compileFile path options
    case result of
      Left err -> errorMessage err >>= fail
      Right result -> do
        lPath <- canonicalizePath path
        includes <- resultIncludes result
                >>= traverse canonicalizePath
                <&> delete lPath
                >>= traverse makeRelativeToCurrentDirectory
        pure (resultString result, includes)

  -- Mark each of these as a dependency.
  traverse_ (fmap (\(_ :: Item ()) -> ()) . load . fromFilePath) deps
  makeItem contents

  where
    options :: SassOptions
    options = def
      { sassOutputStyle = if compress then SassStyleCompressed else SassStyleExpanded
      , sassIncludePaths = Just ["node_modules/normalize.css"]
      }
