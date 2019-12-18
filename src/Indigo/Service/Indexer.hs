module Indigo.Service.Indexer
  ( Handle(..)
  , withHandle
  , rebuild
  , dump
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map

import Control.Monad (forM_)
import Control.Lens ((^.))
import Data.IORef
import Data.Maybe (fromJust)

import Indigo.Page
import qualified Indigo.Service.Repo as Repo
import qualified Indigo.Index as Index

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

newHandle :: IO Handle
newHandle = do
  state <- newIORef Index.clear
  pure
    Handle
      { findAllNames = Index.findAllNames <$> readIORef state
      , findAllTags = Index.findAllTags <$> readIORef state
      , findByName = \name -> Index.findByName name <$> readIORef state
      , findByTag = \tag -> Index.findByTag tag <$> readIORef state
      , update = \name meta -> modifyIORef' state (Index.update name meta)
      , remove = modifyIORef' state . Index.remove
      , clear = writeIORef state Index.clear
      }

withHandle :: (Handle -> IO a) -> IO a
withHandle action = newHandle >>= action

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
