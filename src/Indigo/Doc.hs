{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Indigo.Doc
  ( DocName
  , DocFile
  , DocTag
  , Doc(..)
  , DocMeta(..)
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

type DocTag = T.Text
type DocName = T.Text
type DocFile = T.Text

data DocMeta = DocMeta
  { _name :: DocName
  , _tags :: [T.Text]
  } deriving (Eq, Show)

data Doc = Doc
  { _meta :: !DocMeta
  , _text :: !T.Text
  } deriving (Eq, Show)

text :: Lens' Doc T.Text
text = lens _text $ \d t -> d { _text = t }

meta :: Lens' Doc DocMeta
meta = lens _meta $ \d m -> d { _meta = m }

name :: Lens' DocMeta DocName
name = lens _name $ \d n -> d { _name = n }

tags :: Lens' DocMeta [DocTag]
tags = lens _tags $ \m t -> m { _tags = t }

newDoc :: DocName -> Doc
newDoc name | isValidDocName name = Doc
  { _text = T.unlines ["# " <> name, "", "No contents."]
  , _meta = DocMeta
       { _name = name
       , _tags = []
       }
  }

isValidDocName :: DocName -> Bool
isValidDocName name = True
