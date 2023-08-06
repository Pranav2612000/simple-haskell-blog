module HsBlog.Html.Internal
where

import Numeric.Natural

newtype Html = Html String
newtype Structure = Structure String
newtype Head = Head String
newtype Content = Content String
type Title = String

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure ( getStructureString c1 <> getStructureString c2 )

instance Monoid Structure where
  mempty = empty_

instance Semigroup Content where
  (<>) c1 c2 =
    Content ( getContentString c1 <> getContentString c2 )

instance Monoid Content where
  mempty = Content ""

instance Semigroup Head where
  (<>) h1 h2 =
    Head ( getHeadString h1 <> getHeadString h2 )

instance Monoid Head where
  mempty = Head ""

getStructureString :: Structure -> String
getStructureString content = 
  case content of
    Structure str -> str

getContentString :: Content -> String
getContentString content =
  case content of
    Content cnt -> cnt

getHeadString :: Head -> String
getHeadString htmlhead =
  case htmlhead of
    Head hd -> hd

render :: Html -> String
render html =
  case html of
    Html str -> str

el:: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attr content =
  "<" <> tag <> " " <> attr <> ">" <> content <> "</" <> tag <> ">"

empty_ :: Structure
empty_ = Structure ""

html_ :: Head -> Structure -> Html
html_ htmlhead content =
  Html
    ( el "html"
      (
        el "head" (getHeadString htmlhead)
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

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
  Content ( elAttr "a" ("href=\"" <> escape path <> "\"" ) (getContentString content) )

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\"/>"

b_ :: Content -> Content
b_ = Content . el "b" . getContentString

i_ :: Content -> Content
i_ = Content . el "i" . getContentString

title_ :: String -> Head
title_ title = Head ( el "title" (escape title) )

stylesheet_ :: FilePath -> Head
stylesheet_ sheet = Head ( elAttr "link" ("ref=\"stylesheet\" type=\"text/css\" href=\"" <> sheet <> "\"" ) "" )

meta_ :: String -> String -> Head
meta_ name content = Head ( elAttr "meta" ("name=\"" <> name <> "\" content=\"" <> content <> "\"") "")

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
