module Indigo.Service.Index (Handle(..), withHandle, rebuild, dump) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map

import Control.Monad (forM_)
import Control.Lens ((^.))
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)

import Indigo.Page
import qualified Indigo.Service.Repo as Repo

data Handle = Handle {
  -- inspection
  findAllNames :: IO [T.Text],
  findAllTags :: IO [T.Text],
  findByName :: T.Text -> IO (Maybe PageMeta),
  findByTag :: T.Text -> IO [(T.Text, PageMeta)],

  -- mutation
  clear :: IO (),
  update :: T.Text -> PageMeta -> IO (),
  remove :: T.Text -> IO ()
}

data Index = Index {
  cache :: Map T.Text PageMeta,
  byTag :: Map T.Text [T.Text]
} deriving Show

newHandle :: IO Handle
newHandle = do
  state <- newIORef emptyIndex
  pure
    Handle
      { findAllNames = findAllNames' <$> readIORef state
      , findAllTags = findAllTags' <$> readIORef state
      , findByName = \name -> findByName' name <$> readIORef state
      , findByTag = \tag -> findByTag' tag <$> readIORef state
      , update = \name meta -> modifyIORef' state (update' name meta)
      , remove = modifyIORef' state . remove'
      , clear = writeIORef state emptyIndex
      }
  where
    emptyIndex = Index Map.empty Map.empty

withHandle :: (Handle -> IO a) -> IO a
withHandle action = newHandle >>= action

----- inspection

findAllNames' :: Index -> [T.Text]
findAllNames' Index{..} = Map.keys cache

findAllTags' :: Index -> [T.Text]
findAllTags' Index{..} = Map.keys byTag

findByName' :: T.Text -> Index -> Maybe PageMeta
findByName' name Index{..} = cache !? name

findByTag' :: T.Text -> Index -> [(T.Text, PageMeta)]
findByTag' tag Index{..} = [(name, cache ! name) | name <- fromMaybe [] (byTag !? tag)]

------ mutation
update' :: T.Text -> PageMeta -> Index -> Index
update' name meta index@Index{..} = index'
  where
    index' = index {
      cache = Map.insert name meta cache,
      byTag = foldr (Map.alter alterTags) byTag (meta ^. tags)
    }

    alterTags Nothing = Just [name]
    alterTags (Just tags) = Just (name : filter (/= name) tags)

remove' :: T.Text -> Index -> Index
remove' name index@Index {..} = maybe index index' (cache !? name)
  where
    index' meta = index {
      cache = Map.delete name cache,
      byTag = foldr (Map.alter alterTags) byTag (meta ^. tags)
    }

    alterTags Nothing = Nothing
    alterTags (Just [name]) = Nothing
    alterTags (Just tags) = Just $ filter (/= name) tags

----- utils

rebuild :: Handle -> Repo.Handle -> IO ()
rebuild index repo = do
  names <- Repo.pageIndex repo

  T.putStrLn "Rebuilding index..."
  clear index
  forM_ names $ \name -> do
    page <- fromJust <$> Repo.loadPage repo name
    update index name (page ^. meta)
    T.putStrLn $ "Updated '" <> name <> "'"

dump :: Handle -> IO ()
dump index = do
  tags <- findAllTags index
  forM_ tags $ \tag -> do
    metas <- findByTag index tag
    T.putStr $ "#" <> tag <> ": "
    forM_ metas $ \(name, meta) -> T.putStr $ "'" <> name <> "' "
    T.putStrLn ""
