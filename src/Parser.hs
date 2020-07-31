module Parser
  (
    parserMarkdown
  ) where

import Text.Parsec
import Text.Parsec.String
import Markdown
import Control.Monad ( guard )

parserMarkdown :: Parser Markdown
parserMarkdown = many parseBlock <* eof

parseBlock :: Parser Block
parseBlock = choice [ try parseHeading
                    , try parseQuote
                    , try parseList
                    , try parseDivider
                    , try parseCode
                    , parseParagraph ]

parseHeading :: Parser Block
parseHeading = do
  chars <- many1 $ char '#'
  let charL = length chars
  guard (charL <= 6)
  spaces''
  inlines <- parseInlines
  return $ Headering (toEnum $ pred charL) inlines

parseQuote :: Parser Block
parseQuote = (char '>' <* spaces'') >> (parseInlines >>= return . Quote)

parseList :: Parser Block
parseList = parseOrderedList <|> parseUnorderedList
  where parseOrderedList :: Parser Block
        parseOrderedList = many1 orderedListItem >>= return . List OrderedList

        orderedListItem :: Parser [Inline]
        orderedListItem = ((many1 digit >> char '.') <* spaces'') >> parseInlines

        parseUnorderedList :: Parser Block
        parseUnorderedList = many1 unorderedListItem >>= return . List UnorderedList

        unorderedListItem :: Parser [Inline]
        unorderedListItem = ((oneOf "-*+") <* spaces'') >> parseInlines

parseDivider :: Parser Block
parseDivider = (hyphen <|> asterisk) <* spaces' <* endOfLine
  where hyphen :: Parser Block
        hyphen = do
          chars <- many1 $ char '-'
          guard (length chars >= 3)
          return Divider
        
        asterisk :: Parser Block
        asterisk = do
          chars <- many1 $ char '*'
          guard (length chars >= 3)
          return Divider

parseCode :: Parser Block
parseCode = do
  codeSymbol
  language <- between' spaces' (spaces' <* endOfLine) anyChar
  code <- manyTill anyChar codeSymbol
  spaces'
  endOfLine
  return $ Code (if null language then Nothing else Just language) code
  where codeSymbol :: Parser String
        codeSymbol = count 3 (char '`')

parseParagraph :: Parser Block
parseParagraph = parseInlines >>= return . Paragraph

parseInlines :: Parser [Inline]
parseInlines = manyTill parseInline endOfLine

parseInline :: Parser Inline
parseInline = choice [ try parseLink
                     , try parseImage
                     , try parseStrong
                     , try parseItalic
                     , parseString ]

parseLink :: Parser Inline
parseLink = do
  name <- between' (char '[') (char ']') anyChar
  address <- between' (char '(') (char ')') anyChar
  return $ Link name address

parseImage :: Parser Inline
parseImage = do
  alt <- between' (string "![") (char ']') anyChar
  address <- between' (char '(') (char ')') anyChar
  return $ Image alt address

parseItalic :: Parser Inline
parseItalic = between' asterisk asterisk parseInline >>= return . Italic
  where asterisk = count 1 $ char '*'

parseStrong :: Parser Inline
parseStrong = between' asterisk asterisk parseInline >>= return . Strong
  where asterisk = count 2 $ char '*'

parseString :: Parser Inline
parseString = do
  c <- anyChar
  cs <- manyTill anyChar (lookAhead $ oneOf "![*" <|> endOfLine)
  return $ Text (c:cs)

between' :: Parser a -> Parser b -> Parser c -> Parser [c]
between' a b c = a *> manyTill c (try b)

spaces' :: Parser ()
spaces' = skipMany (char ' ')

spaces'' :: Parser ()
spaces'' = skipMany1 (char ' ')