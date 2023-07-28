import Html

main = putStrLn (render myhtml)

myhtml = 
  html_
    "Page title"
    (
        h1_ "Heading" <>

        (
            p_ "Paragraph #1" <>
            p_ "Paragraph #2"
        )
    )
