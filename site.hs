{-# LANGUAGE OverloadedStrings #-}

import Data.Default

import Text.Sass.Functions

import Hakyll.Core.Configuration
import Hakyll.Web.Sass
import Hakyll

compress :: Bool
compress = False

main :: IO ()
main = hakyllWith def { previewHost = "0.0.0.0"
                      , previewPort = 8080
                      } $ do
    match "assets/*.svg" $ do
        route   idRoute
        compile copyFileCompiler -- TODO: Compress?

    match "assets/main.scss" $ do
        route $ setExtension "css"
        compile $ sassCompilerWith def { sassOutputStyle = if compress then SassStyleCompressed else SassStyleExpanded
                                       , sassImporters = Just [ sassImporter ]
                                       }

    match "index.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate siteCtx
                >>= loadAndApplyTemplate "templates/default.html" siteCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

-- | The default context for the whole site, including site-global
-- properties.
siteCtx :: Context String
siteCtx = defaultContext
       <> constField "site.title" "Amulet ML"
       <> constField "site.description" "Amulet is a simple, functional programming language in the ML tradition"
       <> constField "site.versions.main_css" ""

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    siteCtx


-- | A custom sass importer which also looks within @node_modules@.
sassImporter :: SassImporter
sassImporter = SassImporter 0 go where
  go "normalize" _ = do
    c <- readFile "node_modules/normalize.css/normalize.css"
    pure [ SassImport { importPath = Nothing
                      , importBase = Nothing
                      , importSource = Just c
                      , importSourceMap = Nothing
                      } ]
  go _ _ = pure []
