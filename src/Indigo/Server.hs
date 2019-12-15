{-# LANGUAGE DataKinds #-}

module Indigo.Server where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Maybe (fromJust, fromMaybe)

import Servant
import Servant.JS

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import Indigo.Api as Api
import Indigo.Render
import Text.Blaze.Html5 (Markup, toHtml)
--import Data.Aeson (Value)

import Indigo.Page as Models (Page(..), loadPage, newPage, updatePage, loadOrCreatePage, deletePage)
import Indigo.WikiEnv

uiServer :: WikiEnv -> Server FrontendUi
uiServer env = getPage :<|> postPage
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


apiServer = undefined
--apiServer :: Server BackendApi
--apiServer = getArticle :<|> postArticle :<|> deleteArticle :<|> putArticle :<|> getArticleContent :<|> postArticleContent :<|> getArticleContentHtml
--  where
--    getArticle id = maybe (throwError err404) (return . toArticleDto) =<< liftIO (`dbGetArticle` id)
--
--    postArticle id dto | Indigo.Api.id dto /= Just id = throwError err400 { errBody = "The identifiers in url and body must match." }
--    postArticle id dto = NoContent <$ liftIO (`dbUpdateArticle` fromArticleDto dto)
--
--    deleteArticle id = NoContent <$ liftIO (`dbDeleteArticle` id)
--
--    putArticle dto = NoContent <$ liftIO (`dbCreateArticle` fromArticleDto dto)
--
--    getArticleContent :: Int -> Handler T.Text
--    getArticleContent id = maybe (throwError err404) return =<< liftIO (`dbGetArticleContents` id)
--
--    postArticleContent id contents = do
--      ok <- liftIO $ \conn -> dbUpdateArticleContents conn id contents
--      if ok then return NoContent
--            else throwError err404
--
--    getArticleContentHtml = getArticleContent

type Routes = FrontendUi
              :<|> "api" :> BackendApi
              :<|> "static" :> "api.js" :> Get '[PlainText] T.Text
              :<|> "static" :> Raw

type JsApi' = "api" :> BackendApi

apiJs :: T.Text
apiJs = jsForAPI api jquery
  where
    api = Proxy :: Proxy JsApi'

main :: IO ()
main = run 8080 $ serve routes (uiServer env0 :<|> apiServer :<|> apiJsServer :<|> staticServer)
  where
    routes = Proxy :: Proxy Routes
    staticServer = serveDirectoryWebApp "static"
    apiJsServer = pure apiJs
