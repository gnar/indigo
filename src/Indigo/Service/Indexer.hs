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

import Indigo.Doc
import qualified Indigo.Service.Repo as Repo
import qualified Indigo.Index as Index

data Handle = Handle {
  -- inspection
  findAllNames :: IO [DocName],
  findAllTags :: IO [T.Text],
  findByName :: DocName -> IO (Maybe DocMeta),
  findByTag :: T.Text -> IO [DocMeta],

  -- mutation
  clear :: IO (),
  update :: DocMeta -> IO (),
  remove :: DocName -> IO ()
}

newHandle :: IO Handle
newHandle = do
  state <- newIORef Index.empty
  pure
    Handle
      { findAllNames = Index.findAllNames <$> readIORef state
      , findAllTags = Index.findAllTags <$> readIORef state
      , findByName = \name -> Index.findByName name <$> readIORef state
      , findByTag = \tag -> Index.findByTag tag <$> readIORef state
      , update = modifyIORef' state . Index.update
      , remove = modifyIORef' state . Index.remove
      , clear = writeIORef state Index.empty
      }

withHandle :: (Handle -> IO a) -> IO a
withHandle action = newHandle >>= action

rebuild :: Handle -> Repo.Handle -> IO ()
rebuild index repo = do
  T.putStrLn "Rebuilding index..."

  clear index

  names <- Repo.listDocs repo
  forM_ names $ \name -> do
    meta <- fromJust <$> Repo.loadMeta repo name
    update index meta
    T.putStrLn $ "Updated '" <> name <> "'"

dump :: Handle -> IO ()
dump index = do
  tags <- findAllTags index
  forM_ tags $ \tag -> do
    metas <- findByTag index tag
    T.putStr $ "#" <> tag <> ": "
    forM_ metas $ \meta -> T.putStr $ "'" <> (meta ^. name) <> "' "
    T.putStrLn ""
