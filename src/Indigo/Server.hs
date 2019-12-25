{-# LANGUAGE FlexibleContexts #-}
module Indigo.Server
  ( main 
  ) where

import Control.Lens ((^.), (.~), (&))
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
import qualified Data.ByteString.Lazy as LB

import Indigo.WikiEnv
import Indigo.Api as Api
import Indigo.Doc as Page
import Indigo.Render
import Indigo.Config (guessMimeType)
import qualified Indigo.Service.Repo as Repo
import qualified Indigo.Service.Repo.Impl.FileSystem as RepoFileSystem
import qualified Indigo.Service.Indexer as Indexer

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Functor ((<&>))
import qualified Network.HTTP.Types.Header    as HTTP
import Control.Monad.Error.Class (MonadError)
import System.FilePath ((</>))

frontend :: WikiEnv -> Repo.Handle -> Indexer.Handle -> Server FrontendApi
frontend env repo indexer = listPages :<|> getPage :<|> postPage :<|> getPageFile :<|> listTags :<|> getTag :<|> hmm
  where
    redirectToDoc :: T.Text -> Handler a
    redirectToDoc name = throwError err303 { errHeaders = [(HTTP.hLocation, T.encodeUtf8 $ pageUrl env name) ] }

    listPages :: Handler Markup
    listPages = liftIO $ renderListPages env <$> Indexer.findAllNames indexer

    getPage :: T.Text -> Maybe PageAction -> Handler Markup
    getPage name Nothing = getPage name (Just PageView)
    getPage name (Just PageView) = liftIO $ maybe (renderMissingPage env name) (renderViewPage env) <$> Repo.loadDoc repo name
    getPage name (Just PageEdit) = liftIO $ maybe (renderMissingPage env name) (renderEditPage env) <$> Repo.loadDoc repo name

    getPage name (Just PageCreate) = do
      liftIO $ do
         page <- Repo.loadOrCreateDoc repo name
         Indexer.update indexer (page ^. meta)
      redirectToDoc name

    getPage name (Just PageDelete) = do
      liftIO $ do
        Repo.deleteDoc repo name
        Indexer.remove indexer name
      redirectToDoc name

    getPageFile :: T.Text -> T.Text -> Handler (Headers '[Header "Content-Type" String, Header "Content-Disposition" String] LB.ByteString)
    getPageFile name file =
      liftIO $ do
        stream <- LB.fromStrict . fromJust <$> Repo.loadFile repo name file
        let contentType = guessMimeType (T.unpack file)
            contentDisposition = T.unpack $ "filename=" <> file
        pure $ addHeader contentType $ addHeader contentDisposition stream

    postPage :: T.Text -> PageForm -> Handler Markup
    postPage name form = do
      liftIO $ do
        page <- Repo.loadOrCreateDoc repo name <&> Page.text .~ Api.text form
                                               <&> Page.meta . Page.tags .~ parseTags (Api.tags form)
        Repo.saveDoc repo page
        Indexer.update indexer (page ^. meta)
      redirectToDoc name
      where
        parseTags = filter (not . T.null) . fmap T.strip . T.splitOn ","

    listTags :: Handler Markup
    listTags = liftIO $ renderListTags env <$> Indexer.findAllTags indexer

    getTag :: T.Text -> Handler Markup
    getTag tag = liftIO $ renderGetTag env tag <$> Indexer.findByTag indexer tag

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
