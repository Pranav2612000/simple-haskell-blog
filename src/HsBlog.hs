module HsBlog
  ( convertSingle,
    convertDir
  )
  where

import HsBlog.Html as Html
import HsBlog.Convert

import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDir :: FilePath -> FilePath -> IO ()
convertDir = error "Not implemented"
