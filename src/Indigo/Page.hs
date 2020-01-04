module Indigo.Page where

import qualified Text.Pandoc as P
import qualified Data.Text as T
import Control.Lens (Lens', lens)
import Data.Char
import Data.String
import Data.Maybe (isNothing)

type Tag = T.Text
type Name = T.Text
type Doc = P.Pandoc
type Meta = P.Meta

data Page = Page
  { _name :: Name
  , _meta :: Meta
  , _tags :: [Tag]
  } deriving (Eq, Show)

name :: Lens' Page Name
name = lens _name $ \p n -> p { _name = n }

meta :: Lens' Page Meta
meta = lens _meta $ \p m -> p { _meta = m }

tags :: Lens' Page [Tag]
tags = lens _tags $ \p t -> p { _tags = t }

isValidName :: Name -> Bool
isValidName "tag" = False
isValidName "raw" = False
isValidName "hmm" = False
isValidName name = isStripped && hasNoInvalidChars
  where
    isStripped = name == T.strip name
    hasNoInvalidChars = T.all (`notElem` ("./" :: String)) name
