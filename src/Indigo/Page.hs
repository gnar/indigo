module Indigo.Page where

import qualified Data.Text as T
import Control.Lens (Lens', lens)

import Indigo.Config

type DocName = T.Text

data DocType = DocPage | DocFile deriving (Eq, Show)

data DocMeta = DocMeta
  { _name :: DocName
  , _type :: DocType
  , _tags :: [T.Text]
  } deriving (Show)

data Document =
    Page { _text :: T.Text
         , _meta :: DocMeta
         }
  | File { _file :: T.Text
         , _meta :: DocMeta
         }
  deriving (Show)

text :: Lens' Document T.Text
text = lens _text $ \d t -> d { _text = t }

meta :: Lens' Document DocMeta
meta = lens _meta $ \d m -> d { _meta = m }

name :: Lens' DocMeta DocName
name = lens _name $ \d n -> d { _name = n }

tags :: Lens' DocMeta [T.Text]
tags = lens _tags $ \m t -> m { _tags = t }

newPage :: T.Text -> Document
newPage name = Page
  { _text = T.unlines ["# " <> name, "", "No contents."]
  , _meta = DocMeta
       { _name = name
       , _type = DocPage
       , _tags = []
       }
  }
