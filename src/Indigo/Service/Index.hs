module Indigo.Service.Index (Handle(..), withHandle, rebuild) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad (forM_)
import Control.Lens ((^.))
import Data.IORef
import Data.Maybe (fromJust)

import Indigo.Page
import qualified Indigo.Service.Repo as Repo

data Handle = Handle {
  getTags :: IO [T.Text],
  findByTag :: T.Text -> IO [PageMeta],

  clear :: IO (),
  update :: T.Text -> PageMeta -> IO ()
}

data Index = Index {
  byName :: Map T.Text PageMeta,
  byTag  :: Map T.Text [PageMeta]
} deriving Show

emptyIndex = Index Map.empty Map.empty

newHandle :: IO Handle
newHandle = do
  index <- newIORef emptyIndex
  pure $ Handle {
    getTags = getTags' index,
    findByTag = findByTag' index,
    update = \name meta -> modifyIORef' index (update' name meta),
    clear = writeIORef index emptyIndex
  }

withHandle :: (Handle -> IO a) -> IO a
withHandle action = newHandle >>= action

update' :: T.Text -> PageMeta -> Index -> Index
update' name meta index@Index{..} = index { byName = byName', byTag = byTag' }
  where
    byName' = Map.insert name meta byName
    byTag' = byTag                                           

getTags' :: IORef Index -> IO [T.Text]
getTags' index = do
  byTag <- byTag <$> readIORef index
  pure $ Map.keys byTag

findByTag' :: IORef Index -> T.Text -> IO [PageMeta]
findByTag' index tag = do
  byTag <- byTag <$> readIORef index
  pure $ Map.findWithDefault [] tag byTag


rebuild :: Handle -> Repo.Handle -> IO ()
rebuild index repo = do
  names <- Repo.pageIndex repo

  T.putStrLn "Rebuilding index..."
  clear index
  forM_ names $ \name -> do
    page <- fromJust <$> Repo.loadPage repo name
    update index name (page ^. meta)
    T.putStrLn $ "Updated '" <> name <> "'"

