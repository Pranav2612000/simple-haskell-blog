newtype Html = Html String
newtype Structure = Structure String

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure ( a <> b ) 

el:: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

main = putStrLn myhtml

myhtml = makeHtml "My page title" (h1_ "Welcome" <> p_ "My first paragraph") 
