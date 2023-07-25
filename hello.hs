html_ :: String -> String
html_ body = "<html>" <> body <> "</html>"

body_ :: String -> String
body_ content = "<body>" <> content <> "</body>"

head_ :: String -> String
head_ content = "<head>" <> content <> "</head>"

title_ :: String -> String
title_ title = "<title>" <> title <> "</title>"

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

main = putStrLn myhtml

myhtml = makeHtml "My page title" "My page content"
