module Indigo.Page where

import GHC.Generics
import qualified Data.Text as T
import Control.Lens (Lens', lens)
import Data.Aeson

import Indigo.Config

data Page = Page {
  _name :: T.Text,
  _text :: T.Text,
  _meta :: PageMeta
} deriving (Generic, Show)

name :: Lens' Page T.Text
name = lens _name $ \p n -> p { _name = n }

text :: Lens' Page T.Text
text = lens _text $ \p t -> p { _text = t }

meta :: Lens' Page PageMeta
meta = lens _meta $ \p m -> p { _meta = m }

newtype PageMeta = PageMeta {
  _tags :: [T.Text]
} deriving (Generic, Show)

instance ToJSON PageMeta where
  toJSON = genericToJSON jsonConfig

instance FromJSON PageMeta where
  parseJSON = genericParseJSON jsonConfig

tags :: Lens' PageMeta [T.Text]
tags = lens _tags $ \m t -> m { _tags = t }

newPage :: T.Text -> Page
newPage name = Page {
  _name = name,
  _text = T.unlines [
    "# " <> name,
    "",
    "No contents."
  ],
  _meta = PageMeta []
}
