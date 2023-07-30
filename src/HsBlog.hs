module HsBlog
  ( main
  )
  where

import System.Directory
import System.Environment
import HsBlog.Html
import HsBlog.Convert

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- No arguments: reading from stdin and writing to stdout
    [] -> do
      content <- getContents
      putStrLn (process "Empty title" content)

    -- With input and output file paths as program arguments
    [inputFile, outputFile] -> do
      content <- readFile inputFile
      fileExists <- doesFileExist outputFile
      if fileExists
        then do
          putStrLn "Output file already exists. Overwrite ( Y )"
          answer <- getLine
          case answer of
            "Y" ->
              writeFile outputFile (process inputFile content)
        else do
          writeFile outputFile (process inputFile content)
    _ ->
      putStrLn "Usage: runghc Main.hs [ -- <input-file> <output-file>]"
