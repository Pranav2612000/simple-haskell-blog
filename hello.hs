import System.Directory
import System.Environment
import Html
import Convert

main :: IO ()
-- main = putStrLn (render myhtml)
main =
  getArgs >>= \args ->
    case args of
      -- No arguments: reading from stdin and writing to stdout
      [] ->
        getContents >>= \content ->
          putStrLn (process "Empty title" content)
      _ ->
        putStrLn "Usage: runghc Main.hs [ -- <input-file> <output-file>]"

myhtml = 
  html_
    "Page title"
    (
        h1_ "Heading" <>

        (
            p_ "Paragraph #1" <>
            p_ "Paragraph #2"
        )
    )
