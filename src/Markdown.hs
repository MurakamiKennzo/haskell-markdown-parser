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
           | Code Language Code deriving (Eq, Show)

data HeadingType = H1 | H2 | H3 | H4 | H5 | H6 deriving (Eq, Show, Enum)

data Inline = Link LinkName LinkAddress
            | Image ImageAlt ImageAddress
            | Text String
            | Italic [Inline]
            | Strong [Inline] deriving (Eq, Show)

data ListType = OrderedList | UnorderedList deriving (Eq, Show)

type Markdown = [Block]

type Language = Maybe String
type Code = String
type LinkName = String
type LinkAddress = String
type ImageAlt = String
type ImageAddress = String
