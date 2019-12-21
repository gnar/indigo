module Indigo.Server 
  ( main 
  ) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Maybe (fromJust)

import Servant
import Servant.Multipart
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Text.Blaze.Html5 (Markup, toHtml)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy as LBS

import Indigo.WikiEnv
import Indigo.Api as Api
import Indigo.Page as Page
import Indigo.Render
import qualified Indigo.Service.Repo as Repo
import qualified Indigo.Service.Repo.Impl.FileSystem as RepoFileSystem
import qualified Indigo.Service.Indexer as Indexer

import qualified Data.ByteString.Lazy as BS

frontend :: WikiEnv -> Repo.Handle -> Indexer.Handle -> Server FrontendApi
frontend env repo indexer = listPages :<|> getPage :<|> postPage :<|> getPageFile :<|> listTags :<|> getTag :<|> hmm
  where
    listPages :: Handler Markup
    listPages = do
      names <- liftIO $ Indexer.findAllNames indexer
      pure $ renderListPages env names
    getPage :: T.Text -> Maybe PageAction -> Handler Markup
    getPage name (Just PageCreate) = do
      page <- liftIO $ Repo.loadOrCreateDoc repo name
      liftIO $ Indexer.update indexer (page ^. meta)
      pure $ renderViewPage env page
    getPage name (Just PageView) = do
      page <- liftIO $ Repo.loadDoc repo name
      case page of
        Just page -> pure $ renderViewPage env page
        Nothing -> pure $ renderMissingPage env name
    getPage name (Just PageEdit) = do
      page <- liftIO $ Repo.loadDoc repo name
      case page of
        Just page -> pure $ renderEditPage env page
        Nothing -> pure $ renderMissingPage env name
    getPage name (Just PageDelete) = do
      liftIO $ do
        Repo.deleteDoc repo name
        Indexer.remove indexer name
      pure (renderMissingPage env name)
    getPage name _ = getPage name (Just PageView)
    postPage :: T.Text -> PageForm -> Handler Markup
    postPage name form = do
      page <- liftIO $ Repo.loadOrCreateDoc repo name
      let tags' = filter (not . T.null) (fmap T.strip (T.splitOn "," (Api.tags form)))
          meta' = (page ^. Page.meta) { Page._tags = tags' }
          page' = page { _text = Api.text form
                       , _meta = meta'
                       }
      liftIO $ do
        Repo.saveDoc repo page'
        Indexer.update indexer meta'
      pure $ renderViewPage env page'
    getPageFile name =
      liftIO $ do
        meta <- fromJust <$> Repo.loadMeta repo name
        BS.readFile $ (env ^. pageDir) <> "/" <> (meta ^. file)
    listTags :: Handler Markup
    listTags = do
      tags <- liftIO $ Indexer.findAllTags indexer
      pure $ renderListTags env tags
    getTag :: T.Text -> Handler Markup
    getTag tag = do
      res <- liftIO $ Indexer.findByTag indexer tag
      pure $ renderGetTag env tag res



hmm :: MultipartData Mem -> Handler Markup
hmm multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input)
            ++ " -> " ++ show (iValue input)

    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file
      putStrLn $ "Content of " ++ show (fdFileName file)
      LBS.putStr content
  pure $ "hahaha"

type Routes = FrontendApi :<|> "static" :> Raw

server :: WikiEnv -> Repo.Handle -> Indexer.Handle -> Server Routes
server env repo indexer = frontend env repo indexer :<|> serveDirectoryWebApp (env ^. staticDir)

main :: IO ()
main =
  RepoFileSystem.withHandle (env ^. pageDir) $ \repo ->
    Indexer.withHandle $ \indexer -> do
      Indexer.rebuild indexer repo
      run 8080 $ serve (Proxy :: Proxy Routes) (server env repo indexer)
  where
    env = WikiEnv { _host = "http://localhost:8080"
                  , _pageDir = "pages"
                  , _staticDir = "static"
                  , _mainPage = "Hauptseite"
                  }
