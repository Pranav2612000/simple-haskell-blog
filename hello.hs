import Html

main = putStrLn (render myhtml)

myhtml = 
  html_
    "Page title"
    (
      append_
        (h1_ "Heading")
        (
          append_
            (p_ "Paragraph #1")
            (p_ "Paragraph #2")
        )
    )
