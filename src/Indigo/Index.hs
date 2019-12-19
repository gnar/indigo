module Indigo.Index (
  Index,
  empty,
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
  cache :: Map DocName DocMeta,
  byTag :: Map T.Text [DocName]
} deriving Show

empty :: Index
empty = Index Map.empty Map.empty

findAllNames :: Index -> [DocName]
findAllNames Index{..} = Map.keys cache

findAllTags :: Index -> [T.Text]
findAllTags Index{..} = Map.keys byTag

findByName :: DocName -> Index -> Maybe DocMeta
findByName name Index{..} = cache !? name

findByTag :: T.Text -> Index -> [DocMeta]
findByTag tag Index{..} = [cache ! name | name <- fromMaybe [] (byTag !? tag)]

update :: DocMeta -> Index -> Index
update meta index =
  index'
    { cache = Map.insert name' meta (cache index')
    , byTag = foldr (Map.alter alterTags) (byTag index') (meta ^. tags)
    }
  where
    name' = meta ^. name
    index' = remove name' index
    alterTags Nothing = Just [name']
    alterTags (Just tags) = Just (name' : filter (/= name') tags)

remove :: DocName -> Index -> Index
remove name index@Index {..} = maybe index index' (cache !? name)
  where
    index' meta = index {
      cache = Map.delete name cache,
      byTag = foldr (Map.alter alterTags) byTag (meta ^. tags)
    }
    alterTags Nothing = Nothing
    alterTags (Just [name]) = Nothing
    alterTags (Just tags) = Just $ filter (/= name) tags
