module Markup
  (
    Document,
    Structure(..)
  )
where

import Numeric.Natural
import Data.Maybe

type Document = [Structure]
data Structure
 = Heading Natural String
 | Paragraph String
 | UnorderedList [String]
 | OrderedList [String]
 | CodeBlock [String]
 deriving Show

parse :: String -> Document
parse = parseLines Nothing . lines

trim :: String -> String
trim = unwords . words

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)
        
