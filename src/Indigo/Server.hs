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
import qualified Indigo.Service.Index as Index
import qualified Indigo.Service.Repo as Repo
import qualified Indigo.Service.Repo.FileSystem as RepoFileSystem
import Control.Monad (forM)

import Indigo.Tags (generate)
import Data.Maybe (fromJust)

pages :: WikiEnv -> Repo.Handle -> Server FrontendApi
pages env repo = getPage :<|> postPage
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

type Routes = FrontendApi :<|> "static" :> Raw

server :: WikiEnv -> Repo.Handle -> Server Routes
server env repo = pages env repo :<|> serveDirectoryWebApp (env ^. staticDir)

main :: IO ()
main =
  RepoFileSystem.withHandle (env ^. pageDir) $ \repo -> do
    Index.withHandle $ \index -> do
      Index.rebuild index repo
      Index.dump index
      run 8080 $ serve (Proxy :: Proxy Routes) (server env repo)
  where
    env = WikiEnv {_host = "http://localhost:8080", _pageDir = "pages", _staticDir = "static"}

