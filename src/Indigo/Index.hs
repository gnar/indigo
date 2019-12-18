module Indigo.Index (
  Index,
  clear,
  findAllNames,
  findAllTags,
  findByName,
  findByTag,
  update,
  remove
) where

import qualified Data.Text as T
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map

import Control.Lens ((^.))
import Data.Maybe (fromJust, fromMaybe)

import Indigo.Page

data Index = Index {
  cache :: Map T.Text PageMeta,
  byTag :: Map T.Text [T.Text]
} deriving Show

clear :: Index
clear = Index Map.empty Map.empty

findAllNames :: Index -> [T.Text]
findAllNames Index{..} = Map.keys cache

findAllTags :: Index -> [T.Text]
findAllTags Index{..} = Map.keys byTag

findByName :: T.Text -> Index -> Maybe PageMeta
findByName name Index{..} = cache !? name

findByTag :: T.Text -> Index -> [(T.Text, PageMeta)]
findByTag tag Index{..} = [(name, cache ! name) | name <- fromMaybe [] (byTag !? tag)]

update :: T.Text -> PageMeta -> Index -> Index
update name meta index@Index{..} = index'
  where
    index' = index {
      cache = Map.insert name meta cache,
      byTag = foldr (Map.alter alterTags) byTag (meta ^. tags)
    }

    alterTags Nothing = Just [name]
    alterTags (Just tags) = Just (name : filter (/= name) tags)

remove :: T.Text -> Index -> Index
remove name index@Index {..} = maybe index index' (cache !? name)
  where
    index' meta = index {
      cache = Map.delete name cache,
      byTag = foldr (Map.alter alterTags) byTag (meta ^. tags)
    }

    alterTags Nothing = Nothing
    alterTags (Just [name]) = Nothing
    alterTags (Just tags) = Just $ filter (/= name) tags
