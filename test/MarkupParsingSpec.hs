{-# language QuasiQuotes #-}

module MarkupParsingSpec where

import Test.Hspec
import Text.RawString.QQ
import HsBlog.Markup

example3 :: String
example3 = [r|
Remember that multiple lines with no seperation are grouped together
into a single paragraph but list items remains seperate

# Item 1
# Item 2
|]

example3Result :: Document
example3Result = 
  [ Paragraph "Remember that multiple lines with no seperation are grouped together into a single paragraph but list items remains seperate"
  , OrderedList
    [ "Item 1"
    , "Item 2"
    ]
  ]

spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    simple

simple :: Spec
simple = do
  describe "simple" $ do
    it "empty" $
      shouldBe
        (parse "")
        []

    it "paragraph" $ do
      shouldBe
        (parse "hello world")
        [Paragraph "hello world"]

    it "heading 1" $ do
      shouldBe
        (parse "* Heading 1")
        [Heading 1 "Heading 1"]

    it "code" $ do
      shouldBe
        (parse "> main = putStrLn \"hello world\"")
        [CodeBlock ["main = putStrLn \"hello world\""]]

multiline :: Spec
multiline = do
  describe "Multi-line tests" $ do
    it "example3" $ do
      shouldBe
        (parse example3)
        example3Result


