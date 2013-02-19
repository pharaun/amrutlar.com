--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Prelude              hiding (catch)
import           Control.Applicative  ((<$>))
import           Control.Monad        (ap)
import           Data.Monoid          (mappend, mconcat)
import qualified Data.Map             as M
import qualified Data.List            as L
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString      as S
import           Hakyll
import           System.IO.Temp       (openBinaryTempFile)
import           System.Cmd           (rawSystem)
import           System.IO            (hFlush, hClose, openBinaryFile, IOMode(ReadMode))
import           System.IO.Error      (isDoesNotExistError)
import           System.Directory     (removeFile)
import           Control.Exception    (throwIO, catch)
import qualified Text.Pandoc          as P
import qualified Text.Pandoc.Generic  as PG
import           Data.Char            (toLower)
import           System.Process       (readProcess)
import           System.IO.Unsafe     (unsafePerformIO)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- Special case the favicon.ico to website root
    match "images/favicon.ico" $ do
        route $ gsubRoute "images/" (const "")
        compile copyFileCompiler

    -- Special case the background
    match "images/boston-skyline.png" $ do
        route $ setExtension "jpg"
        compile $ getResourceLBS
            >>= convertToJPG
            >>= crushJPG

    -- Copy over the remaining images
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- Should only output scss files without a underscore in front (ie6.scss, ie.scss, standard.scss)
    match (fromRegex "^scss/[^_][^/]*.scss") $ do
        route $ gsubRoute "scss" (const "css") `composeRoutes` setExtension ".css"
        compile $ getResourceString >>= sassify

    -- Copy over and apply the basic template for the basic pages
    match (fromList ["static/about.html", "static/resume.html"]) $ do
        route $ gsubRoute "static/" (const "")
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Build tags
    tags <- buildTags "articles/*" (fromCapture "tags/*.html")

    -- Generate the project and article relevant pages
    match "projects/*" $ do
        route $ setExtension "html"
        compile $ pandocPygmentizeCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/project.html" projectCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "articles/*" $ do
        route $ traditionalArticle `composeRoutes` setExtension "html"
        compile $ pandocPygmentizeCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/article.html" (articleCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Generate the index for the project and articles
    create ["projects.html"] $ do
        route idRoute
        compile $ do
            let listCtx = mconcat
                    [ field "projects" (\_ -> projectList chronological)
                    , constField "title" "Projects"
                    , constField "menu" "projects"
                    , projectCtx
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/project-list.html" listCtx
                >>= loadAndApplyTemplate "templates/default.html" listCtx
                >>= relativizeUrls

    create ["articles.html"] $ do
        route idRoute
        compile $ do
            let listCtx = mconcat
                    [ field "articles" (\_ -> articleList tags "articles/*" recentFirst)
                    , constField "title" "Articles"
                    , constField "menu" "blog"
                    , articleCtx tags
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/article-list.html" listCtx
                >>= loadAndApplyTemplate "templates/default.html" listCtx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Category: " ++ tag

        -- Copied from posts, need to refactor
        route $ gsubRoute "tags" (const "categories")
        compile $ do
            let tagCtx = mconcat
                    [ field "articles" (\_ -> articleList tags pattern recentFirst)
                    , constField "title" title
                    , constField "menu" "blog"
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/article-tags.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" (defaultContext `mappend` constField "menu" "blog")
                >>= relativizeUrls

    -- Generate the templates
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
fancyDateCtx :: Context String
fancyDateCtx =
    dateField "date" "<div class=\"postDate\"><span class=\"day\">%d</span><span class=\"month\">%b</span><span class=\"year\">%Y</span></div>"

--------------------------------------------------------------------------------
articleCtx :: Tags -> Context String
articleCtx tags = mconcat
    [ fancyDateCtx
    , tagsField "tags" tags
    , defaultContext
    ]

--------------------------------------------------------------------------------
projectCtx :: Context String
projectCtx = mconcat
    [ field "sources" compileSources
    , field "licenses" compileLicenses
    , defaultContext
    ]

--------------------------------------------------------------------------------
summaryCtx :: Context String
summaryCtx =
    field "summary" (\item -> return $ head $ lines $ itemBody item)

--------------------------------------------------------------------------------
traditionalArticle :: Routes
traditionalArticle = customRoute cleanDate

cleanDate :: Identifier -> FilePath
cleanDate i = L.concat $ L.intersperse "/" $ [L.head path] ++ yearDay ++ (L.tail $ L.init path) ++ [fileName]
    where
        path = splitAll "/" $ toFilePath i
        fileName = L.concat $ L.intersperse "-" $ L.drop 3 $ splitAll "-" $ L.last path
        yearDay = L.take 2 $ splitAll "-" $ L.last path

--------------------------------------------------------------------------------
-- TODO: fix this up to use proper templates, but fuckit it works as it is right now
compileSources :: Item a -> Compiler String
compileSources item = do
    metadata <- getMetadata (itemIdentifier item)
    return $ case M.lookup "sources" metadata of
        Just x    -> unlines . map (ddA . trim) $ splitAll "," x
        otherwise -> "<dd>None</dd>"
    where ddA = ("<dd><a href=\"" ++) . ap (++) (("\">" ++) . (++ "</a></dd>"))

--------------------------------------------------------------------------------
-- TODO: fix this up to use proper templates, but fuckit it works as it is right now
compileLicenses :: Item a -> Compiler String
compileLicenses item = do
    metadata <- getMetadata (itemIdentifier item)
    return $ case M.lookup "licenses" metadata of
        Just x    -> unlines . map ((\i -> case M.lookup i license of
                Just x    -> x
                otherwise -> "<dd>None</dd>"
            ) . trim) $ splitAll "," x
        otherwise -> "<dd>None</dd>"
    where
        -- TODO: extract to a file and load it at compile time
        license = M.fromList [
            ("simplified-bsd", "<dd><a href=\"http://www.opensource.org/licenses/bsd-license.php\">Simplified BSD</a></dd>"),
            ("gfdl", "<dd><a href=\"http://www.gnu.org/copyleft/fdl.html\">GNU Free Documentation License</a></dd>"),
            ("gplv2", "<dd><a href=\"http://www.gnu.org/licenses/gpl-2.0.html\">GPLv2</a></dd>"),
            ("lgplv2", "<dd><a href=\"http://www.gnu.org/licenses/lgpl-2.1.html\">LGPLv2.1</a></dd>")
            ]

--------------------------------------------------------------------------------
articleList :: Tags -> Pattern -> ([Item String] -> [Item String]) -> Compiler String
articleList tags pattern sortFilter = do
    articles   <- sortFilter <$> loadAllSnapshots pattern "content"
    itemTpl <- loadBody "templates/article-item.html"
    list    <- applyTemplateList itemTpl (summaryCtx `mappend` articleCtx tags) articles
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

--------------------------------------------------------------------------------
-- Convert then crush the background jpeg
convertToJPG item = withItemBody (unixFilterLBS "convert" ["-", "-quality", "100", "jpg:-"]) item >>= return

--------------------------------------------------------------------------------
-- TODO: Need to clean up the file handling big time here
-- Some of this manual handles can probably be replaced with writeFile
crushJPG :: Item B.ByteString -> Compiler (Item B.ByteString)
crushJPG item = unsafeCompiler $ do
    (filePath, handle) <- openBinaryTempFile "/tmp/" "crush"
    B.hPut handle (itemBody item)
    hFlush handle
    hClose handle

    -- Now run jpeg optim on the temporary file
    rawSystem "jpegoptim" ["-m80", filePath]

    -- Read it all back in non-lazy and return it
    newHandle <- openBinaryFile filePath ReadMode
    content <- S.hGetContents newHandle
    hClose newHandle

    -- Remove file
    removeIfExists filePath

    -- Convert to lazy and returnA
    return $ itemSetBody (B.fromChunks [content]) item

--------------------------------------------------------------------------------
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where
        handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

--------------------------------------------------------------------------------
pandocPygmentizeCompiler :: Compiler (Item String)
pandocPygmentizeCompiler =
    pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions highlight

highlight :: P.Pandoc -> P.Pandoc
highlight = (PG.bottomUp highlightBlock :: P.Pandoc -> P.Pandoc)

highlightBlock :: P.Block -> P.Block
highlightBlock (P.CodeBlock (_, options , _ ) code) = P.RawBlock "html" (pygments code options)
highlightBlock x = x

-- TODO: Extremely messy, borrowed from: https://github.com/lcw/jocco/blob/master/docs/pygments.hs
pygments :: String -> [String] -> String
pygments code options
    | (length options) == 1 = cleanupDivs $ unsafePerformIO $ readProcess "pygmentize" ["-l", (map toLower (head options)), "-O encoding=utf8,outencoding=utf8", "-f", "html"] code
    | (length options) == 2 = cleanupDivs $ unsafePerformIO $ readProcess "pygmentize" ["-l", (map toLower (head options)), "-O encoding=utf8,outencoding=utf8", "-f", "html"] code
    | otherwise = "<pre class =\"highlight\"><code>" ++ code ++ "</code></pre>"

-- INPUT:  <div class="highlight"><pre> ++ code ++ </pre></div>
-- OUTPUT: <pre class ="highlight"><code> ++ code ++ </code></pre>
-- TODO: Do this in a better way than regex such as parsing and rebuilding the html
cleanupDivs :: String -> String
cleanupDivs x =
    replaceAll "<div class=\"" (const "<pre class=\"") $
    replaceAll "\"><pre>" (const "\"><code>") $
    replaceAll "</pre></div>" (const "</code></pre>") x
