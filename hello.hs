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
      -- With input and output file paths as program arguments
      [inputFile, outputFile] ->
        readFile inputFile >>= \content ->
          doesFileExist outputFile >>= \fileExists ->
            case fileExists of
              False ->
                writeFile outputFile (process inputFile content)
              True ->
                putStrLn "Output file already exists. Overwrite ( Y/N )" *>
                  getLine >>= \answer ->
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
