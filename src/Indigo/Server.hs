{-# LANGUAGE MultiParamTypeClasses #-}
module Indigo.Server
  ( runServer
  ) where

import Control.Lens ((^.), (.~), (&))
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Maybe (fromJust)

import Servant
import Servant.Multipart
import Text.Blaze.Html               (Html, ToMarkup, toHtml)

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Text.Blaze.Html5 (Markup, toHtml)
import Network.Wai.Handler.Warp (run, runSettings, defaultSettings, setHost, setPort)
import qualified Data.ByteString.Lazy as LB

import Indigo.Api as Api
import Indigo.Urls
import Indigo.Environment
import Indigo.Render
import Indigo.Resources (staticFiles)
import qualified Indigo.Service.Repo as Repo
import qualified Indigo.Service.Ops as Ops
import qualified Indigo.Service.Indexer as Indexer

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Functor ((<&>))
import qualified Network.HTTP.Types.Header    as HTTP
import Control.Monad.Error.Class (MonadError)
import System.FilePath ((</>), takeExtension, dropExtension)

import Text.Pandoc as P
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.Streaming.Network (HostPreference)
import Data.String (fromString)

frontend :: Environment -> Repo.Handle -> Indexer.Handle -> Server FrontendApi
frontend env repo indexer =
  downloadFile :<|> getTags :<|> getTag :<|> hmm :<|> getPages :<|> getPage :<|> postPage
  where
    getPages =
      liftIO $ renderListPages env <$> Indexer.findAllPages indexer

    getPage name Nothing =
      getPage name (Just PageView)

    getPage name (Just PageView) =
      liftIO $ render <$> Ops.loadPage repo name
      where
        render (Just (page, pandoc, _)) = renderViewPage env (page, pandoc)
        render Nothing                  = let (page, _, text) = Ops.newPage name in renderEditPage env (page, text)

    getPage name (Just PageEdit) =
      liftIO $ render <$> Ops.loadPage repo name
      where
        render (Just (page, _, text))   = renderEditPage env (page, text)
        render Nothing                  = let (page, _, text) = Ops.newPage name in renderEditPage env (page, text)

    getPage name (Just PageDelete) = do
      liftIO $ Ops.deletePage repo name
      redirect env (Api.buildPageLink name Nothing)

    postPage name form = do
      liftIO $ do
        (page, _, _) <- Ops.savePage repo name (Api.text form)
        Indexer.update indexer page
      redirect env (Api.buildPageLink name Nothing)

    downloadFile path =
      liftIO $ do
        stream <- LB.fromStrict . fromJust <$> Repo.loadFile repo path
        let contentType = guessMimeType path
            contentDisposition = "filename=" <> path
        pure $ addHeader contentType $ addHeader contentDisposition stream

    getTags =
      liftIO $ renderListTags env <$> Indexer.findAllTags indexer

    getTag tag =
      liftIO $ renderGetTag env tag <$> Indexer.findByTag indexer tag

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
      LB.putStr content
  pure "hahaha"

guessMimeType :: FilePath -> String
guessMimeType f
  | ext `elem` [".jpg", "*.jpeg"] = "image/jpeg"
  | ext == ".png" = "image/png"
  | ext == ".md" = "text/markdown"
  | otherwise = "data/octet-stream"
  where
    ext = toLower (takeExtension f)
    toLower = T.unpack . T.toLower . T.pack

redirect :: Environment -> Link -> Handler a
redirect env link = throwError err303 { errHeaders = [(HTTP.hLocation, T.encodeUtf8 . T.pack . show $ buildURI env link ) ] }

---

type Routes = FrontendApi :<|> "static" :> Raw

server :: Environment -> Repo.Handle -> Indexer.Handle -> Server Routes
server env repo indexer = frontend env repo indexer :<|> serveDirectoryEmbedded staticFiles

runServer :: Environment -> IO ()
runServer env =
  Repo.withHandle (env ^. envStore) $ \repo ->
    Indexer.withHandle $ \indexer -> do
      Indexer.rebuild indexer repo
      runSettings settings $ serve (Proxy :: Proxy Routes) (server env repo indexer)
  where
    settings = defaultSettings
                 & setHost (fromString (env ^. envHost))
                 & setPort (fromIntegral (env ^. envPort))
