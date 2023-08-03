module HsBlog
  ( convertSingle,
    convertDir,
    buildIndex
  )
  where

import HsBlog.Html as Html
import HsBlog.Convert
import HsBlog.Directory(convertDirectory, buildIndex)

import System.IO

convertDir :: FilePath -> FilePath -> IO()
convertDir = convertDirectory

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)
