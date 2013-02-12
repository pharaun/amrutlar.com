--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Monad       (ap)
import           Data.Monoid         (mappend)
import qualified Data.Map            as Map
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- Just copy over all of the images
    -- TODO: Special case the favicon.ico it should be in website root
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
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Generate all of the relevant files for projects related pages
    match "projects/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
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
                    articleCtx

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
    field "summary" (\item -> return $ head $ lines $ itemBody item)

--------------------------------------------------------------------------------
projectCtx :: Context String
projectCtx =
    field "sources" compileSources `mappend`
    field "licenses" compileLicenses `mappend`
    defaultContext

--------------------------------------------------------------------------------
-- TODO: fix this up to use proper templates, but fuckit it works as it is right now
compileSources :: Item a -> Compiler String
compileSources item = do
    metadata <- getMetadata (itemIdentifier item)
    return $ case Map.lookup "sources" metadata of
        Just x    -> unlines . map (ddA . trim) $ splitAll "," x
        otherwise -> "<dd>None</dd>"
    where ddA = ("<dd><a href=\"" ++) . ap (++) (("\">" ++) . (++ "</a></dd>"))

--------------------------------------------------------------------------------
-- TODO: fix this up to use proper templates, but fuckit it works as it is right now
compileLicenses :: Item a -> Compiler String
compileLicenses item = do
    metadata <- getMetadata (itemIdentifier item)
    return $ case Map.lookup "licenses" metadata of
        Just x    -> unlines . map ((\i -> case Map.lookup i license of
                Just x    -> x
                otherwise -> "<dd>None</dd>"
            ) . trim) $ splitAll "," x
        otherwise -> "<dd>None</dd>"
    where
        -- TODO: extract to a file and load it at compile time
        license = Map.fromList [
            ("simplified-bsd", "<dd><a href=\"http://www.opensource.org/licenses/bsd-license.php\">Simplified BSD</a></dd>"),
            ("gfdl", "<dd><a href=\"http://www.gnu.org/copyleft/fdl.html\">GNU Free Documentation License</a></dd>"),
            ("gplv2", "<dd><a href=\"http://www.gnu.org/licenses/gpl-2.0.html\">GPLv2</a></dd>"),
            ("lgplv2", "<dd><a href=\"http://www.gnu.org/licenses/lgpl-2.1.html\">LGPLv2.1</a></dd>")
            ]

--------------------------------------------------------------------------------
articleList :: ([Item String] -> [Item String]) -> Compiler String
articleList sortFilter = do
    articles   <- sortFilter <$> loadAllSnapshots "articles/*" "content"
    itemTpl <- loadBody "templates/article-item.html"
    list    <- applyTemplateList itemTpl (summaryCtx `mappend` articleCtx) articles
    return list

--------------------------------------------------------------------------------
projectList :: ([Item String] -> [Item String]) -> Compiler String
projectList sortFilter = do
    projects   <- sortFilter <$> loadAllSnapshots "projects/*" "content"
    itemTpl <- loadBody "templates/project-item.html"
    list    <- applyTemplateList itemTpl (summaryCtx `mappend` projectCtx) projects
    return list

--------------------------------------------------------------------------------
-- Run sass, then compress - TODO: make the library path more generic
sassify item = withItemBody (unixFilter "sass" ["-s", "--scss", "--trace", "--load-path", "scss", "--load-path", "scss/compass", "--load-path", "scss/lib", "-r", "./scss/lib/constants.rb", "-r", "./scss/lib/gradient_support.rb"]) item >>= return -- . fmap compressCss
