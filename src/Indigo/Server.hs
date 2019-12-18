module Indigo.Server ( main ) where

import Control.Monad.IO.Class
import qualified Data.Text as T

import Control.Lens ((^.))

import Servant
import Text.Blaze.Html5 (Markup, toHtml)
import Network.Wai.Handler.Warp (run)

import Indigo.WikiEnv
import Indigo.Api as Api
import Indigo.Page as Page
import Indigo.Render
import qualified Indigo.Service.Indexer as Indexer
import qualified Indigo.Service.Repo as Repo
import qualified Indigo.Service.Repo.Impl.FileSystem as RepoFileSystem
import Control.Monad (forM)

import Indigo.Tags (generate)
import Data.Maybe (fromJust)

frontend :: WikiEnv -> Repo.Handle -> Indexer.Handle -> Server FrontendApi
frontend env repo indexer = getPage :<|> postPage :<|> getTag
  where
    getPage :: T.Text -> Maybe PageAction -> Handler Markup
    getPage name (Just PageCreate) = do
      liftIO $ Repo.pageIndex repo >>= print
      page <- liftIO $ Repo.loadOrCreatePage repo name
      pure $ renderViewPage env page
    getPage name (Just PageView) = do
      page <- liftIO $ Repo.loadPage repo name
      case page of
        Just page -> pure $ renderViewPage env page
        Nothing -> pure $ renderMissingPage env name
    getPage name (Just PageEdit) = do
      page <- liftIO $ Repo.loadPage repo name
      case page of
        Just page -> pure $ renderEditPage env page
        Nothing -> pure $ renderMissingPage env name
    getPage name (Just PageDelete) = do
      () <- liftIO $ Repo.deletePage repo name
      pure (renderMissingPage env name)
    getPage name _ = getPage name (Just PageView)
    postPage :: T.Text -> PageForm -> Handler Markup
    postPage name form = do
      page <- liftIO $ Repo.loadOrCreatePage repo name
      let page' = page {Page._text = Api.text form}
      liftIO $ Repo.updatePage repo page'
      pure $ renderViewPage env page'
    getTag :: T.Text -> Handler Markup
    getTag tag = do
      res <- liftIO $ Indexer.findByTag indexer tag
      pure $ renderViewTag env tag res

type Routes = FrontendApi :<|> "static" :> Raw

server :: WikiEnv -> Repo.Handle -> Indexer.Handle -> Server Routes
server env repo indexer = frontend env repo indexer :<|> serveDirectoryWebApp (env ^. staticDir)

main :: IO ()
main =
  RepoFileSystem.withHandle (env ^. pageDir) $ \repo ->
    Indexer.withHandle $ \indexer -> do
      Indexer.rebuild indexer repo
      Indexer.dump indexer
      run 8080 $ serve (Proxy :: Proxy Routes) (server env repo indexer)
  where
    env = WikiEnv {_host = "http://localhost:8080", _pageDir = "pages", _staticDir = "static"}
