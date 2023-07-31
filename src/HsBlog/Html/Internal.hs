module HsBlog.Html.Internal
where

import Numeric.Natural

newtype Html = Html String
newtype Structure = Structure String
newtype Content = Content String
type Title = String

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure ( getStructureString c1 <> getStructureString c2 )

instance Monoid Structure where
  mempty = empty_

getStructureString :: Structure -> String
getStructureString content = 
  case content of
    Structure str -> str

getContentString :: Content -> String
getContentString content =
  case content of
    Content cnt -> cnt

render :: Html -> String
render html =
  case html of
    Html str -> str

el:: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

empty_ :: Structure
empty_ = Structure ""

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      (
        el "head" (el "title" (escape title))
        <>
        el "body" (getStructureString content)
      )
    )

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat . map (el "li" . getStructureString)

escape :: String -> String
escape = 
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar

concatStructure :: [Structure] -> Structure
concatStructure list =
  case list of
    [] -> empty_
    x: xs -> x <> concatStructure xs
