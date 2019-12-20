module Indigo.Page
  ( DocName
  , DocType(..)
  , Doc(..)
  , DocMeta(..)
  , newPage
  , text
  , meta
  , name
  , tags
  ) where

import qualified Data.Text as T
import Control.Lens (Lens', lens)

import Indigo.Config

type DocTag = T.Text
type DocName = T.Text

data DocType = DocTypePage | DocTypeFile deriving (Eq, Show)

data DocMeta = DocMeta
  { _name :: DocName
  , _type :: DocType
  , _tags :: [T.Text]
  } deriving (Show)

data Doc =
    DocPage { _text :: T.Text
            , _meta :: DocMeta
            }
  | DocFile { _file :: T.Text
            , _meta :: DocMeta
            }
  deriving (Show)

text :: Lens' Doc T.Text
text = lens _text $ \d t -> d { _text = t }

meta :: Lens' Doc DocMeta
meta = lens _meta $ \d m -> d { _meta = m }

name :: Lens' DocMeta DocName
name = lens _name $ \d n -> d { _name = n }

tags :: Lens' DocMeta [DocTag]
tags = lens _tags $ \m t -> m { _tags = t }

newPage :: DocName -> Doc
newPage name = DocPage
  { _text = T.unlines ["# " <> name, "", "No contents."]
  , _meta = DocMeta
       { _name = name
       , _type = DocTypePage
       , _tags = []
       }
  }
