module Indigo.Doc
  ( DocName
  , DocType(..)
  , Doc(..)
  , DocMeta(..)
  , newDocPage
  , text
  , meta
  , name
  , file
  , tags
  , isValidDocName
  ) where

import qualified Data.Text as T
import Control.Lens (Lens', lens)

import Indigo.Config

type DocTag = T.Text
type DocName = T.Text

data DocType = DocTypePage | DocTypeImage deriving (Eq, Show)

data DocMeta = DocMeta
  { _name :: DocName
  , _file :: FilePath
  , _type :: DocType
  , _tags :: [T.Text]
  } deriving (Eq, Show)

data Doc =
    DocPage { _text :: T.Text
            , _meta :: DocMeta
            }
  | DocImage { _meta :: DocMeta
             }
  deriving (Eq, Show)

text :: Lens' Doc T.Text
text = lens _text $ \d t -> d { _text = t }

meta :: Lens' Doc DocMeta
meta = lens _meta $ \d m -> d { _meta = m }

name :: Lens' DocMeta DocName
name = lens _name $ \d n -> d { _name = n }

file :: Lens' DocMeta FilePath
file = lens _file $ \d f -> d { _file = f }

tags :: Lens' DocMeta [DocTag]
tags = lens _tags $ \m t -> m { _tags = t }

newDocPage :: DocName -> Doc
newDocPage name | isValidDocName name = DocPage
  { _text = T.unlines ["# " <> name, "", "No contents."]
  , _meta = DocMeta
       { _name = name
       , _file = T.unpack $ name <> ".md"
       , _type = DocTypePage
       , _tags = []
       }
  }

isValidDocName name = True
