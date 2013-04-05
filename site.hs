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
main = hakyllWith config $ do
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
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= relativizeUrls

    -- Build tags
    tags <- buildTags "articles/*" (fromCapture "tags/*.html")

    -- Generate the project and article relevant pages
    match "projects/*" $ do
        route $ setExtension "html"
        compile $ pandocPygmentizeCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/project.html" projectCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= relativizeUrls

    match "articles/*" $ do
        route $ traditionalArticle `composeRoutes` setExtension "html"
        compile $ pandocPygmentizeCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/article.html" (articleCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= relativizeUrls

    -- Generate the index for the project and articles
    create ["projects.html"] $ do
        route idRoute
        compile $ do
            let listCtx = mconcat
                    [ field "projects" (\_ -> projectList)
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
                    , defaultCtx
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/article-tags.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" (defaultCtx `mappend` constField "menu" "blog")
                >>= relativizeUrls

    -- Feeds
    create ["rss.xml"] $ do
        route idRoute
        compile $ loadAllSnapshots "articles/*" "content"
            >>= recentFirst
            >>= renderAtom feedConfiguration feedCtx

    -- Generate the templates
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
fancyDateCtx :: Context String
fancyDateCtx =
    dateField "date" "<div class=\"postDate\"><span class=\"day\">%d</span><span class=\"month\">%b</span><span class=\"year\">%Y</span></div>"

--------------------------------------------------------------------------------
defaultCtx :: Context String
defaultCtx = mconcat
    [ field "mathjax" mathjax
    , defaultContext
    ]

--------------------------------------------------------------------------------
articleCtx :: Tags -> Context String
articleCtx tags = mconcat
    [ fancyDateCtx
    , tagsField "tags" tags
    , defaultCtx
    ]

--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

--------------------------------------------------------------------------------
projectCtx :: Context String
projectCtx = mconcat
    [ field "sources" compileSources
    , field "licenses" compileLicenses
    , defaultCtx
    ]

--------------------------------------------------------------------------------
summaryCtx :: Context String
summaryCtx =
    field "summary" (\item -> return $ head $ lines $ itemBody item)

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -avz ./_site/* amrutlar.com:/var/www/"
    }

--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Amrutlar"
    , feedDescription = "Articles on various technical topic"
    , feedAuthorName = "Anja Berens"
    , feedAuthorEmail = "pharaun666@gmail.com"
    , feedRoot = "http://amrutlar.com"
    }

--------------------------------------------------------------------------------
-- Only load mathjax if there is actually math on the page, indicated in
-- the metadata option "math: true"
-- Originally found on John Lenz's blog:
-- http://blog.wuzzeb.org/posts/2012-06-08-hakyll-and-latex.html
-- and adapted to work with Hakyll 4
mathjax :: Item String -> Compiler String
mathjax item = do
    metadata <- getMetadata (itemIdentifier item)
    return $ case M.lookup "math" metadata of
        Just "true" -> "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />"
        otherwise -> ""

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
            ("bsd3", "<dd><a href=\"http://opensource.org/licenses/BSD-3-Clause\">BSD 3-Clause License</a></dd>"),
            ("gfdl", "<dd><a href=\"http://www.gnu.org/copyleft/fdl.html\">GNU Free Documentation License</a></dd>"),
            ("gplv2", "<dd><a href=\"http://www.gnu.org/licenses/gpl-2.0.html\">GPLv2</a></dd>"),
            ("agplv3", "<dd><a href=\"http://www.gnu.org/licenses/agpl-3.0.html\">AGPLv3</a></dd>"),
            ("lgplv2", "<dd><a href=\"http://www.gnu.org/licenses/lgpl-2.1.html\">LGPLv2.1</a></dd>")
            ]

--------------------------------------------------------------------------------
articleList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
articleList tags pattern sortFilter = do
    itemTpl <- loadBody "templates/article-item.html"
    articles <- sortFilter =<< loadAllSnapshots pattern "content"
    applyTemplateList itemTpl (summaryCtx `mappend` articleCtx tags) articles

--------------------------------------------------------------------------------
projectList :: Compiler String
projectList = do
    itemTpl <- loadBody "templates/project-item.html"
    projects <- loadAllSnapshots "projects/*" "content"
    applyTemplateList itemTpl (summaryCtx `mappend` projectCtx) projects

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
mathJaxPandocOptions :: P.WriterOptions
mathJaxPandocOptions = defaultHakyllWriterOptions
    { P.writerHTMLMathMethod = P.MathJax ""
    }

--------------------------------------------------------------------------------
pandocPygmentizeCompiler :: Compiler (Item String)
pandocPygmentizeCompiler =
    pandocCompilerWithTransform defaultHakyllReaderOptions mathJaxPandocOptions highlight

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
