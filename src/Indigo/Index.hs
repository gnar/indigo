module Indigo.Index (
  Index,
  empty,
  findAllPages,
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
import Data.Maybe (fromMaybe)

import Indigo.Page

data Index = Index {
  cache :: Map Name Page,
  byTag :: Map T.Text [Name]
} deriving Show

empty :: Index
empty = Index Map.empty Map.empty

findAllPages :: Index -> [Page]
findAllPages Index{..} = Map.elems cache

findAllTags :: Index -> [T.Text]
findAllTags Index{..} = Map.keys byTag

findByName :: Name -> Index -> Maybe Page
findByName name Index{..} = cache !? name

findByTag :: T.Text -> Index -> [Page]
findByTag tag Index{..} = [cache ! name | name <- fromMaybe [] (byTag !? tag)]

update :: Page -> Index -> Index
update page index =
  index'
    { cache = Map.insert name' page (cache index')
    , byTag = foldr (Map.alter alterTags) (byTag index') (page ^. tags)
    }
  where
    name' = page ^. name
    index' = remove name' index
    alterTags Nothing = Just [name']
    alterTags (Just tags) = Just (name' : filter (/= name') tags)

remove :: Name -> Index -> Index
remove name index@Index {..} = maybe index index' (cache !? name)
  where
    index' page = index {
      cache = Map.delete name cache,
      byTag = foldr (Map.alter alterTags) byTag (page ^. tags)
    }
    alterTags Nothing = Nothing
    alterTags (Just [name]) = Nothing
    alterTags (Just tags) = Just $ filter (/= name) tags
