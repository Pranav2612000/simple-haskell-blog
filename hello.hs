html_ body = "<html>" <> body <> "</html>"
body_ content = "<body>" <> content <> "</body>"
head_ content = "<head>" <> content <> "</head>"
title_ title = "<title>" <> title <> "</title>"

wrapHtml content = "<html><body>" <> content <> "</body></html>"
main = putStrLn (html_ (body_ myhtml))

myhtml = "Hello World"
