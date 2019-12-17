module Indigo.Tags where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Tuple (swap)

import Indigo.Page
import Control.Lens ((^.))

newtype Tags = Tags {
  _index :: M.Map T.Text [T.Text]
} deriving Show

generate :: [(T.Text, PageMeta)] -> Tags
generate = Tags . M.fromListWith (++) . foldMap (\(name, meta) -> [(tag, [name]) | tag <- meta ^. tags])
