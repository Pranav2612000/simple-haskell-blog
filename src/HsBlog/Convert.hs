module HsBlog.Convert where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import qualified HsBlog.Env as Env
import Data.Foldable

convert :: Html.Title -> Env.Env -> Markup.Document -> Html.Html
convert title (Env.Env _ sheet) = Html.html_ (Html.title_ title <> Html.stylesheet_ sheet) . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n $ Html.txt_ txt

    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list

    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)

process :: Html.Title -> Env.Env -> String -> String
process title env = Html.render . convert title env . Markup.parse

