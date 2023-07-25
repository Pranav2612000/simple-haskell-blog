wrapHtml content = "<html><body>" <> content <> "</body></html>"
main = putStrLn (wrapHtml myhtml)

myhtml = "Hello World"
