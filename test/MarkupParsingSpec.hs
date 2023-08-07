module MarkupParsingSpec where

import Test.Hspec
import HsBlog.Markup

spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    it "empty" $
      shouldBe
        (parse "")
        []

    it "paragraph" $
      shouldBe
        (parse "hello world")
        [Paragraph "hello world"]

    it "heading 1" $
      shouldBe
        (parse "* Heading 1")
        [Heading 1 "Heading 1"]

    it "code" $
      shouldBe
        (parse "> main = putStrLn \"hello world\"")
        [CodeBlock ["main = putStrLn \"hello world\""]]
