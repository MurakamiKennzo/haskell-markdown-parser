module Html
  (
    genHtml
  ) where

import Markdown

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
htmlBlock (Code Nothing code) = "<pre><code>" <> text code <> "</code></pre>"
htmlBlock (Code (Just language) code) = "<pre><code class=\"" <> language <> "\">" <> text code <> "</code></pre>"

htmlInlines :: [Inline] -> Html
htmlInlines [] = ""
htmlInlines (x:xs) = htmlInline x <> htmlInlines xs

htmlInline :: Inline -> Html
htmlInline (Link name address) = "<a href=\"" <> address <> "\">" <> text name <> "</a>"
htmlInline (Image alt address) = "<img src=\"" <> address <> "\" alt=\"" <> alt <> "\" />"
htmlInline (Text str) = text str
htmlInline (Italic xs) = "<em>" <> htmlInlines xs <> "</em>"
htmlInline (Strong xs) = "<strong>" <> htmlInlines xs <> "</strong>"

type Html = String

text :: String -> String
text = concatMap char
  where char :: Char -> String
        char '<' = "&lt;"
        char '>' = "&gt;"
        char '&' = "&amp;"
        char '"' = "&quot;"
        char '\'' = "&#39;"
        char x = [x]
