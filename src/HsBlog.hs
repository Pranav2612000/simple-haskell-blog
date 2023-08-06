module HsBlog
  ( convertSingle,
    convertDir,
    Env,
    defaultEnv,
    buildIndex
  )
  where

import HsBlog.Html as Html
import HsBlog.Env (Env(..), defaultEnv)
import HsBlog.Convert
import HsBlog.Directory(convertDirectory, buildIndex)

import System.IO

convertDir :: Env -> FilePath -> FilePath -> IO()
convertDir = convertDirectory

convertSingle :: Env -> Html.Title -> Handle -> Handle -> IO ()
convertSingle env title input output = do
  content <- hGetContents input
  hPutStrLn output (process title env content)
