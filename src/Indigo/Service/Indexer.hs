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
import qualified Indigo.Service.Ops as Ops
import qualified Indigo.Service.Repo as Repo
import qualified Indigo.Index as Index
import Data.Functor ((<&>))

data Handle = Handle {
  -- inspection
  findAllPages :: IO [Page],
  findAllTags :: IO [Tag],
  findByName :: Name -> IO (Maybe Page),
  findByTag :: Tag -> IO [Page],

  -- mutation
  clear :: IO (),
  update :: Page -> IO (),
  remove :: Name -> IO ()
}

newHandle :: IO Handle
newHandle = do
  state <- newIORef Index.empty
  pure
    Handle
      { findAllPages = readIORef state <&> Index.findAllPages
      , findAllTags = readIORef state <&> Index.findAllTags
      , findByName = \name -> readIORef state <&> Index.findByName name
      , findByTag = \tag -> readIORef state <&> Index.findByTag tag
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

  names <- Repo.listFiles repo <&> fmap fst . filter (\(name, file) -> file == "_text.md")
  forM_ names $ \name -> do
    (page, _, _) <- fromJust <$> Ops.loadPage repo name
    update index page
    T.putStrLn $ "Updated '" <> name <> "'"

dump :: Handle -> IO ()
dump index = do
  tags <- findAllTags index
  forM_ tags $ \tag -> do
    metas <- findByTag index tag
    T.putStr $ "#" <> tag <> ": "
    forM_ metas $ \meta -> T.putStr $ "'" <> (meta ^. name) <> "' "
    T.putStrLn ""
