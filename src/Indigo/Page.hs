module Indigo.Page
  ( PageName
  , PageFile
  , Tag
  , Page(..)
  , Meta(..)
  , newDoc
  , text
  , meta
  , name
  , tags
  , isValidDocName
  ) where

import qualified Data.Text as T
import Control.Lens

import Indigo.Config

type Tag = T.Text
type PageName = T.Text
type PageFile = T.Text

data Meta = Meta
  { _name :: PageName
  , _tags :: [T.Text]
  } deriving (Eq, Show)

data Page = Page
  { _meta :: !Meta
  , _text :: !T.Text
  } deriving (Eq, Show)

text :: Lens' Page T.Text
text = lens _text $ \d t -> d { _text = t }

meta :: Lens' Page Meta
meta = lens _meta $ \d m -> d { _meta = m }

name :: Lens' Meta PageName
name = lens _name $ \d n -> d { _name = n }

tags :: Lens' Meta [Tag]
tags = lens _tags $ \m t -> m { _tags = t }

newDoc :: PageName -> Page
newDoc name | isValidDocName name = Page
  { _text = T.unlines ["# " <> name, "", "No contents."]
  , _meta = Meta
       { _name = name
       , _tags = []
       }
  }

isValidDocName :: PageName -> Bool
isValidDocName name = True
