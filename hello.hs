import System.Directory
import System.Environment
import Html
import Convert

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
      case fileExists of
        False -> do
          writeFile outputFile (process inputFile content)
        True -> do
          putStrLn "Output file already exists. Overwrite ( Y )"
          answer <- getLine
          case answer of
            "Y" ->
              writeFile outputFile (process inputFile content)
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
