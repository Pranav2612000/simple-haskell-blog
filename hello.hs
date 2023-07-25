html_ body = "<html>" <> body <> "</html>"
body_ content = "<body>" <> content <> "</body>"
head_ content = "<head>" <> content <> "</head>"
title_ title = "<title>" <> title <> "</title>"

makeHtml title content = html_ (head_ (title_ title) <> body_ content)

main = putStrLn myhtml

myhtml = makeHtml "My page title" "My page content"
