{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashSet as HSet
import qualified Data.Text as T
import Data.Default
import Data.List
import Data.Char

import qualified Text.HTML.TagSoup as TS

import Text.Sass.Functions

import Hakyll.Core.Configuration
import Hakyll.Web.Sass
import Hakyll

import qualified Skylighting as S

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

    match "assets/main.scss" $ do
        route $ setExtension "css"
        compile $ sassCompilerWith def { sassOutputStyle = if compress then SassStyleCompressed else SassStyleExpanded
                                       , sassImporters = Just [ sassImporter ]
                                       }

    -- TODO: Use typescript instead?
    match "assets/main.js" $ do
        route $ setExtension "js"
        compile $ getResourceString

    match ("assets/**.png" .||. "*.ico") $ do
        route   idRoute
        compile copyFileCompiler

    match "index.html" $ do
        route idRoute
        compile $ getResourceBody
             >>= applyAsTemplate siteCtx
             >>= highlightAmulet
             >>= loadAndApplyTemplate "templates/default.html" siteCtx
             >>= compresses minifyHtml

    match "templates/*" $ compile templateBodyCompiler

-- | The default context for the whole site, including site-global
-- properties.
siteCtx :: Context String
siteCtx = defaultContext
       <> constField "site.title" "Amulet ML"
       <> constField "site.description" "Amulet is a simple, functional programming language in the ML tradition"

       <> field "site.versions.main_css" (const . hashCompiler . fromFilePath $ "assets/main.scss")
       <> field "site.versions.main_js"  (const . hashCompiler . fromFilePath $ "assets/main.js")

-- | A custom sass importer which also looks within @node_modules@.
sassImporter :: SassImporter
sassImporter = SassImporter 0 go where
  go "normalize" _ = do
    c <- readFile "node_modules/normalize.css/normalize.css"
    pure [ SassImport { importPath = Nothing
                      , importAbsolutePath = Nothing
                      , importSource = Just c
                      , importSourceMap = Nothing
                      } ]
  go _ _ = pure []

-- | Looks around for blocks marked as @data-language="amulet"@ and
-- highlight them.
--
-- This uses the OCaml highligher from Skylight for now (which is what
-- Pandoc uses), but we will move this to use the Amulet compiler in the
-- future.
highlightAmulet :: Item String -> Compiler (Item String)
highlightAmulet = pure . fmap (withTagList walk) where
  walk [] = []
  walk (o@(TS.TagOpen "pre" attrs):TS.TagText src:c@(TS.TagClose "pre"):xs)
    | elem ("data-language", "amulet") attrs
    = o : highlight src ++ c:walk xs
  walk (x:xs) = x:walk xs

  -- | Normalise a highlighted block to trim trailing/leading line and remove the indent.
  dropIndent :: T.Text -> T.Text
  dropIndent t =
    let lines = dropWhile (T.all isSpace) . dropWhileEnd (T.all isSpace) . T.lines $ t
        indent = minimum . map (T.length . T.takeWhile isSpace) $ lines
    in T.intercalate "\n" . map (T.drop indent) $ lines

  highlight :: String -> [TS.Tag String]
  highlight txt =
    let Just syntax = S.lookupSyntax "Objective Caml" S.defaultSyntaxMap
        Right lines = S.tokenize (S.TokenizerConfig S.defaultSyntaxMap False) syntax . dropIndent . T.pack $ txt
    in foldr (flip (foldr mkElement . (TS.TagText "\n":))) [] lines

  mkElement :: S.Token -> [TS.Tag String] -> [TS.Tag String]
  mkElement (ty, txt) xs
    = TS.TagOpen "span" [("class", "tok-" ++ tokName ty)]
    : TS.TagText (T.unpack txt)
    : TS.TagClose "span"
    : xs

  tokName :: S.TokenType -> String
  tokName t =
    let name = show t
    in map toLower . take (length name - 3) $ name

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
    , "sub", "sup", "svg", "textarea", "time", "tt", "u", "var", "wbr"
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
