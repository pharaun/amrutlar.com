--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- Just copy over all of the images
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- Should only output the following 3 files: ie6.css, ie.css, standard.css
    match (fromRegex "^scss/[^_][^/]*.scss") $ do
        route $ gsubRoute "scss" (const "css") `composeRoutes` setExtension ".css"
        compile $ getResourceString >>= sassify

    -- Copy over and apply the basic template for the basic pages
    match (fromList ["about.html", "resume.html"]) $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Generate all of the relevant files for projects related pages
    match "projects/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/project.html" projectCtx
            >>= loadAndApplyTemplate "templates/default.html" projectCtx
            >>= relativizeUrls

    create ["projects.html"] $ do
        route idRoute
        compile $ do
            let listCtx =
                    field "projects" (\_ -> projectList recentFirst) `mappend`
                    constField "title" "Projects" `mappend`
                    constField "menu" "projects" `mappend`
                    projectCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/project-list.html" listCtx
                >>= loadAndApplyTemplate "templates/default.html" listCtx
                >>= relativizeUrls

    -- Generate all of the relevant files for article related pages
    match "articles/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/article.html" articleCtx
            >>= loadAndApplyTemplate "templates/default.html" articleCtx
            >>= relativizeUrls

    create ["articles.html"] $ do
        route idRoute
        compile $ do
            let listCtx =
                    field "articles" (\_ -> articleList recentFirst) `mappend`
                    constField "title" "Articles" `mappend`
                    constField "menu" "blog" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/article-list.html" listCtx
                >>= loadAndApplyTemplate "templates/default.html" listCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
articleCtx :: Context String
articleCtx =
    dateField "date" "<div class=\"postDate\"><span class=\"day\">%d</span><span class=\"month\">%b</span><span class=\"year\">%Y</span></div>" `mappend`
    defaultContext

--------------------------------------------------------------------------------
summaryCtx :: Context String
summaryCtx =
    field "summary" (\item -> return $ head $ lines $ itemBody item) `mappend`
    articleCtx

--------------------------------------------------------------------------------
projectCtx :: Context String
projectCtx =
    defaultContext

--------------------------------------------------------------------------------
articleList :: ([Item String] -> [Item String]) -> Compiler String
articleList sortFilter = do
    articles   <- sortFilter <$> loadAllSnapshots "articles/*" "content"
    itemTpl <- loadBody "templates/article-item.html"
    list    <- applyTemplateList itemTpl summaryCtx articles
    return list

--------------------------------------------------------------------------------
projectList :: ([Item String] -> [Item String]) -> Compiler String
projectList sortFilter = do
    projects   <- sortFilter <$> loadAll "projects/*"
    itemTpl <- loadBody "templates/project-item.html"
    list    <- applyTemplateList itemTpl projectCtx projects
    return list

--------------------------------------------------------------------------------
-- Run sass, then compress - TODO: make the library path more generic
sassify item = withItemBody (unixFilter "sass" ["-s", "--scss", "--trace", "--load-path", "scss", "--load-path", "scss/compass", "--load-path", "scss/lib", "-r", "./scss/lib/constants.rb", "-r", "./scss/lib/gradient_support.rb"]) item >>= return -- . fmap compressCss
