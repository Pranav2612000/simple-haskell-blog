module HsBlog.Directory
  ( convertDirectory
  )
where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (process, convert, convertStructure)

import Data.List ( partition )
import Data.Traversable (for)
import Control.Monad (void, when)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeException(..))
import System.Exit (exitFailure)
import System.FilePath
  ( takeExtension
  , takeBaseName
  , (<.>)
  , (</>)
  , takeFileName
  )
import System.Directory
  ( createDirectory
  , removeDirectoryRecursive
  , listDirectory
  , doesDirectoryExist
  , copyFile
  )

convertDirectory :: FilePath -> FilePath -> IO()
convertDirectory inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let
    outputHtmls = txtsToRenderedHtml filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."

data DirContents =
  DirContents
  { dcFilesToProcess :: [(FilePath, String)]
  , dcFilesToCopy :: [FilePath]
  }

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let
    (txtFiles, otherFiles) =
      partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $ DirContents
    { dcFilesToProcess = txtFilesAndContent
    , dcFilesToCopy = otherFiles
    }

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList fn list =
  traverse
    (\file -> do
      maybeResult <-
        catch
          (Right <$> fn file)
          ( \(SomeException e) -> do
            pure $ Left (displayException e)
          )
      pure (file, maybeResult)
    )
    list

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap (\(file, contentOrErr) -> 
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]
    )

createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
        then do
          override <- confirm "Output directory exists. Override (Y/N)?"
          when override (removeDirectoryRecursive dir)
          pure override
        else
          pure True
  when create (createDirectory dir)
  pure create

-- txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
-- txtsToRenderedHtml =
--   map $ \(file, content) ->
--     let
--       updatedFile = (takeBaseName file <> ".html")
--       htmlContent = process (takeBaseName file) content
--     in
--       (updatedFile, htmlContent)
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml txtFiles =
  let
    txtOutputFiles = map toOutputMarkupFile txtFiles
    index = ("index.html", buildIndex txtOutputFiles)
  in
    map (fmap Html.render) (index : map convertFile txtOutputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (file, doc) = (file, convert file doc)

confirm :: String -> IO Bool
confirm question = do
  putStrLn question
  answer <- getLine
  case answer of
    "Y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. Use Y or n"
      confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()
