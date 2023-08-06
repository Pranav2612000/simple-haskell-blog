module OptParse
  ( Options(..)
  , SingleInput(..)
  , SingleOutput(..)
  , parse
  )
  where

import Options.Applicative
import Data.Maybe (fromMaybe)
import HsBlog.Env


data Options
  = ConvertSingle SingleInput SingleOutput Env
  | ConvertDir FilePath FilePath Env
  deriving Show

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show

pSingleInput :: Parser SingleInput
pSingleInput = fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput = fromMaybe Stdout <$> optional pOutputFile

pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

pOutputFile :: Parser SingleOutput
pOutputFile = fmap OutputFile parser
  where
    parser =
      strOption
        ( long "output"
          <> short 'i'
          <> metavar "FILE"
          <> help "Output file"
        )

pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput <*> pEnv

pInputDir :: Parser FilePath
pInputDir = 
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "DIRECTORY"
      <> help "Input directory"
    )

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "DIRECTORY"
      <> help "Output directory"
    )

pEnv :: Parser Env
pEnv = Env <$> pBlogName <*> pStylesheetPath
pBlogName :: Parser String
pBlogName =
  strOption
    ( long "blogname"
      <> short 'b'
      <> metavar "BLOG NAME"
      <> help "Blog name"
      <> value (eBlogName defaultEnv)
      <> showDefault
    )

pStylesheetPath :: Parser String
pStylesheetPath = 
  strOption
    ( long "stylesheet"
      <> short 's'
      <> metavar "STYLES"
      <> help "Stylesheet path"
      <> value (eStylesheetPath defaultEnv)
      <> showDefault
    )

pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir <*> pEnv

pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
  info
    (helper <*> pConvertSingle)
    (progDesc "Convert a single markup source to html")

pConvertDirInfo =
  info
    (helper <*> pConvertDir)
    (progDesc "Convert a directory of markup files to html")

pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand =
  command "convert" pConvertSingleInfo

pConvertDirCommand =
  command "convert-dir" pConvertDirInfo

pOptions :: Parser Options
pOptions =
  subparser (
    pConvertSingleCommand <> pConvertDirCommand
  )

opts :: ParserInfo Options
opts =
  info
    (helper <*> pOptions)
    ( fullDesc
      <> header "hs-blog-gen : A static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )

parse :: IO Options
parse = execParser opts
