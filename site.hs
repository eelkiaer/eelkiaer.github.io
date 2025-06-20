--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--import           Data.Monoid (mappend)
import           Hakyll

import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import Control.Monad ((>=>))
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Network.URI.Encode as URI (encode)
import qualified Data.Text as Text

---------------------------------------------------------------------------------

myPandocCompiler = pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions $ walkM tikzFilter

tikzFilter :: Block -> Compiler Block
tikzFilter (CodeBlock (id, "tikzpicture":extraClasses, namevals) contents) =
  (imageBlock . ("data:image/svg+xml;utf8," ++) . URI.encode . filter (/= '\n') . itemBody <$>) $
    makeItem (Text.unpack contents)
     >>= loadAndApplyTemplate (fromFilePath "templates/tikz.tex") (bodyField "body")
     >>= withItemBody (return . pack
                       >=> unixFilterLBS "rubber-pipe" ["--pdf"]
                       >=> unixFilterLBS "pdftocairo" ["-svg", "-", "-"]
                       >=> return . unpack)
  where imageBlock fname = Para [Image (id, "tikzpicture":extraClasses, namevals) [] (Text.pack fname, "")]
tikzFilter x = return x

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

--    match (fromList ["about.md", "contact.md"]) $ do
    match "about.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/add_title.html" defaultContext
            >>= loadAndApplyTemplate "templates/frame.html" defaultContext
            >>= relativizeUrls


    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/add_title.html" defaultContext
            >>= loadAndApplyTemplate "templates/frame.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let blogCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/add_title.html" blogCtx
                >>= loadAndApplyTemplate "templates/frame.html" blogCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/frame.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
