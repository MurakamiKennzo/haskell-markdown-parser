module Parser
  (
    parserMarkdown
  , parseImage
  , parseLink
  , parseInlines
  , parseInline
  ) where

import Text.Parsec
import Text.Parsec.String
import Markdown
import Control.Monad ( guard )

parserMarkdown :: Parser Markdown
parserMarkdown = many parseBlock

parseBlock :: Parser Block
parseBlock = choice [ try parseHeading
                    , try parseQuote
                    , try parseList
                    , try parseDivider
                    , try parseCode
                    , try parseParagraph ]

parseHeading :: Parser Block
parseHeading = do
  chars <- many1 $ char '#'
  let charL = length chars
  guard (charL <= 6)
  char ' '
  inlines <- parseInlines
  newline'
  return $ Headering (toEnum $ pred charL) inlines

parseQuote :: Parser Block
parseQuote = do
  char '>'
  char ' '
  inlines <- parseInlines
  newline'
  return $ Quote inlines

parseList :: Parser Block
parseList = try parseOrderedList <|> try parseUnorderedList
  where parseOrderedList :: Parser Block
        parseOrderedList = do
          many1 $ digit
          char '.'
          char ' '
          item <- parseInlines
          newline'
          items <- parseOrderedList'
          return $ List OrderedList (item:items)
        
        parseOrderedList' :: Parser [[Inline]]
        parseOrderedList' = choice [ try newline' >> return []
                                   , parseOrderedList'' ]

        parseOrderedList'' :: Parser [[Inline]]
        parseOrderedList'' = do
          many1 $ digit
          char '.'
          char ' '
          item <- parseInlines
          newline'
          items <- parseOrderedList'
          return $ item : items
          
        parseUnorderedList :: Parser Block
        parseUnorderedList = do
          digit <- char '-'
          char ' '
          item <- parseInlines
          newline'
          items <- parseUnorderedList'
          return $ List UnoerderedList (item:items)
        
        parseUnorderedList' :: Parser [[Inline]]
        parseUnorderedList' = choice [ try newline' >> return []
                                     , parseUnorderedList'' ]

        parseUnorderedList'' :: Parser [[Inline]]
        parseUnorderedList'' = do
          digit <- char '-'
          char ' '
          item <- parseInlines
          newline'
          items <- parseUnorderedList'
          return $ item : items

parseDivider :: Parser Block
parseDivider = do
  chars <- many1 $ char '-'
  guard (length chars >= 3)
  newline'
  return $ Divider

parseCode :: Parser Block
parseCode = do
  string "```"
  newline'
  code <- many anyChar
  newline'
  string "```"
  newline'
  return $ Code code

parseParagraph :: Parser Block
parseParagraph = do
  inlines <- parseInlines
  newline'
  return $ Paragraph inlines

parseInlines :: Parser [Inline]
parseInlines = manyTill parseInline newline'

parseInline :: Parser Inline
parseInline = choice [ try parseLink
                     , try parseImage
                     , try parseItalicStrong
                     , try parseStrong
                     , try parseItalic
                     , try parseString ]

parseLink :: Parser Inline
parseLink = do
  char '['
  name <- manyTill anyChar (char ']')
  char '('
  address <- manyTill anyChar (char ')')
  return $ Link name address

parseImage :: Parser Inline
parseImage = do
  char '!'
  char '['
  alt <- manyTill anyChar (char ']')
  char '('
  address <- manyTill anyChar (char ')')
  return $ Image alt address

parseItalic :: Parser Inline
parseItalic = do
  char '*'
  str <- manyTill anyChar (char '*')
  return $ Italic str

parseStrong :: Parser Inline
parseStrong = do
  string "**"
  str <- manyTill anyChar (string "**")
  return $ Strong str

parseItalicStrong :: Parser Inline
parseItalicStrong = do
  string "***"
  str <- manyTill anyChar (string "***")
  return $ ItalicStrong str

parseString :: Parser Inline
parseString = do
  str <- manyTill anyChar (lookAhead $ newline' <|> (nextInlineOrEnd >> return ()))
  return $ Text str
  where nextInlineOrEnd :: Parser Inline
        nextInlineOrEnd = choice [ try parseLink
                                 , try parseImage
                                 , try parseItalicStrong
                                 , try parseStrong
                                 , try parseItalic ]

newline' = eof <|> (newline >> return ())
