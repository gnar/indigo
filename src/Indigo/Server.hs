module Indigo.Server ( main ) where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Maybe (fromJust, fromMaybe)

import Control.Lens ((^.))

import Servant
import Text.Blaze.Html5 (Markup, toHtml)
import Network.Wai.Handler.Warp (run)

import Indigo.Api as Api
import Indigo.Render as Render
import Indigo.Page as Models
import Indigo.WikiEnv as Env

pages :: WikiEnv -> Server FrontendApi
pages env = getPage :<|> postPage
  where
    getPage :: T.Text -> Maybe PageAction -> Handler Markup
    getPage name (Just PageCreate) = do
      page <- liftIO $ loadOrCreatePage env name
      pure $ renderViewPage env page
    getPage name (Just PageView) = do
      page <- liftIO $ loadPage env name
      case page of
        Just page -> pure $ renderViewPage env page
        Nothing -> pure $ renderMissingPage env name
    getPage name (Just PageEdit) = do
      page <- liftIO $ loadPage env name
      case page of
        Just page -> pure $ renderEditPage env page
        Nothing -> pure $ renderMissingPage env name
    getPage name (Just PageDelete) = do
      () <- liftIO $ deletePage env name
      pure (renderMissingPage env name)
    getPage name _ = getPage name (Just PageView)

    postPage :: T.Text -> PageForm -> Handler Markup
    postPage name form = do
      page <- liftIO $ loadOrCreatePage env name
      let page' = page { Models._text = Api.text form }
      seq page' (liftIO $ updatePage env page')
      pure $ renderViewPage env page'

type Routes = FrontendApi :<|> "static" :> Raw

server :: WikiEnv -> Server Routes
server env = pages env :<|> serveDirectoryWebApp (T.unpack $ env ^. staticDir)

main :: IO ()
main = run 8080 $ serve (Proxy :: Proxy Routes) (server env)
  where
    env = WikiEnv { _host = "http://localhost:8080"
                  , _pageDir = "pages"
                  , _staticDir = "static"
                  }
