--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Maybe (fromMaybe)
import           Control.Applicative


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    match (fromList ["favicon.ico", "humans.txt", "robots.txt"]) $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.markdown", "contact.markdown", "projects.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" pageCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- latestPosts
            let indexContext =
                    listField "posts" (postTeaserContext "content") (return posts) `mappend`
                    constField "title" "Home"                                      `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext

--------------------------------------------------------------------------------

latestPosts :: Compiler [Item String]
latestPosts = recentFirst =<< loadAllSnapshots "posts/*" "content"

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postTeaserContext :: String -> Context String
postTeaserContext snapshot = teaserField "teaser" snapshot `mappend` postCtx

pageCtx :: Context String
pageCtx = 
    defaultContext
