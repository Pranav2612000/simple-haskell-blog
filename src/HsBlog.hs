module HsBlog
  ( convertSingle,
    convertDir
  )
  where

import HsBlog.Html as Html
import HsBlog.Markup as Markup
import HsBlog.Convert

import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDir :: FilePath -> FilePath -> IO ()
convertDir = error "Not implemented"

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let
    content =
      map
        ( \(file, doc) ->
          case doc of
            Markup.Heading 1 heading : article ->
              Html.h_ 3 ( Html.link_ file (Html.txt_ heading) )
                <> foldMap convertStructure ( take 3 article )
                <> Html.p_ ( Html.link_ file (Html.txt_ "..."))
            _ ->
              Html.h_ 3 (Html.link_ file (Html.txt_ file))
        )
        files
  in
    Html.html_
      "Home" 
      ( Html.h_ 1 (Html.link_ "index.html" ( Html.txt_ "Blog" ) )
        <> Html.h_ 2 ( Html.txt_ "Posts" )
        <> mconcat content 
      )
