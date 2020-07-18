module Markdown
  (
    Markdown
  , Block(..)
  , Inline(..)
  , ListType(..)
  , HeadingType
  ) where

data Block = Paragraph [Inline]
           | Headering HeadingType [Inline]
           | Quote [Inline]
           | List ListType [[Inline]]
           | Divider
           | Code String deriving (Eq, Show)

data HeadingType = H1 | H2 | H3 | H4 | H5 | H6 deriving (Eq, Show, Enum)

data Inline = Link LinkName LinkAddress
            | Image ImageAlt ImageAddress
            | Text String
            | Italic String
            | Strong String
            | ItalicStrong String deriving (Eq, Show)

data ListType = OrderedList | UnoerderedList deriving (Eq, Show)

type Markdown = [Block]

type LinkName = String
type LinkAddress = String
type ImageAlt = String
type ImageAddress = String
