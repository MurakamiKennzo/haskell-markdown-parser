module Html
  (
    genHtml
  ) where

import Markdown
import HTMLEntities.Text
import Data.Text ( pack
                 , unpack )

genHtml :: Markdown -> Html
genHtml [] = ""
genHtml (x:xs) = htmlBlock x <> genHtml xs

htmlBlock :: Block -> Html
htmlBlock (Paragraph xs) = "<p>" <> htmlInlines xs <> "</p>"
htmlBlock (Headering t xs) = "<h" <> show (fromEnum (succ t)) <> ">"
                                  <> htmlInlines xs
                                  <> "</h" <> show (fromEnum (succ t)) <> ">"
htmlBlock (Quote xs) = "<blockquote>" <> htmlInlines xs <> "</blockquote>"
htmlBlock (List OrderedList xs) = "<ol>" <> concatMap (\x -> "<li>" <> htmlInlines x <> "</li>") xs <> "</ol>"
htmlBlock (List UnorderedList xs) = "<ul>" <> concatMap (\x -> "<li>" <> htmlInlines x <> "</li>") xs <> "</ul>"
htmlBlock Divider = "<hr />"
htmlBlock (Code code) = "<pre><code>" <> unpack (text . pack $ code) <> "</code></pre>"

htmlInlines :: [Inline] -> Html
htmlInlines [] = ""
htmlInlines (x:xs) = htmlInline x <> htmlInlines xs

htmlInline :: Inline -> Html
htmlInline (Link name address) = "<a href=\"" <> address <> "\">" <> unpack (text . pack $ name) <> "</a>"
htmlInline (Image alt address) = "<img src=\"" <> address <> "\" alt=\"" <> alt <> "\" />"
htmlInline (Text str) = unpack .text . pack $ str
htmlInline (Italic str) = "<em>" <> unpack (text . pack $ str) <> "</em>"
htmlInline (Strong str) = "<strong>" <> unpack (text . pack $ str) <> "</strong>"
htmlInline (ItalicStrong str) = "<em><Strong>" <> unpack (text . pack $ str) <> "</strong></em>"

type Html = String

