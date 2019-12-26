{-# LANGUAGE FlexibleContexts #-}
module Indigo.Server
  ( main 
  ) where

import Control.Lens ((^.), (.~), (&))
import Control.Monad (forM_)
import Control.Monad.IO.Class
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
import Indigo.Page as Page
import Indigo.Render
import qualified Indigo.Service.Repo as Repo
import qualified Indigo.Service.Ops as Ops
import qualified Indigo.Service.Indexer as Indexer

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Functor ((<&>))
import qualified Network.HTTP.Types.Header    as HTTP
import Control.Monad.Error.Class (MonadError)
import System.FilePath ((</>), takeExtension)

renderEditNewPage env name =
    renderEditPage env (newPage, newText)
  where
    (newPage, _, newText) = Ops.newPage name

frontend :: WikiEnv -> Repo.Handle -> Indexer.Handle -> Server FrontendApi
frontend env repo indexer = listPages :<|> getPage :<|> postPage :<|> getFile :<|> listTags :<|> getTag :<|> hmm
  where
    redirectToDoc :: T.Text -> Handler a
    redirectToDoc name = throwError err303 { errHeaders = [(HTTP.hLocation, T.encodeUtf8 $ pageUrl env name) ] }

    listPages :: Handler Markup
    listPages = liftIO $ renderListPages env <$> Indexer.findAllPages indexer

    getPage :: T.Text -> Maybe PageAction -> Handler Markup
    getPage name (Just PageView) = liftIO $ Ops.loadPage repo name <&> maybe (renderEditNewPage env name) (\(a, b, _) -> renderViewPage env (a, b))
    getPage name (Just PageEdit) = liftIO $ Ops.loadPage repo name <&> maybe (renderEditNewPage env name) (\(a, _, b) -> renderEditPage env (a, b))
    getPage name (Just PageDelete) = liftIO (Ops.deletePage repo name) >> redirectToDoc name
    getPage name _ = getPage name (Just PageView)

    postPage :: T.Text -> PageForm -> Handler Markup
    postPage name form = do
      liftIO $ do
        (Just (page, _, _)) <- Ops.savePage repo name (Api.text form)
        Indexer.update indexer page
      redirectToDoc name
      where
        parseTags = filter (not . T.null) . fmap T.strip . T.splitOn ","

    getFile :: T.Text -> T.Text -> Handler (Headers '[Header "Content-Type" String, Header "Content-Disposition" String] LB.ByteString)
    getFile name file =
      liftIO $ do
        stream <- LB.fromStrict . fromJust <$> Repo.loadFile repo name file
        let contentType = guessMimeType (T.unpack file)
            contentDisposition = T.unpack $ "filename=" <> file
        pure $ addHeader contentType $ addHeader contentDisposition stream

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

guessMimeType :: FilePath -> String
guessMimeType f
  | ext `elem` [".jpg", "*.jpeg"] = "image/jpeg"
  | ext == ".png" = "image/png"
  | ext == ".md" = "text/markdown"
  | otherwise = "data/octet-stream"
  where
    ext = toLower (takeExtension f)
    toLower = T.unpack . T.toLower . T.pack

type Routes = FrontendApi :<|> "static" :> Raw

server :: WikiEnv -> Repo.Handle -> Indexer.Handle -> Server Routes
server env repo indexer = frontend env repo indexer :<|> serveDirectoryWebApp (envStaticDir env)

main :: IO ()
main =
  Repo.withHandle (envPageDir env) $ \repo ->
    Indexer.withHandle $ \indexer -> do
      Indexer.rebuild indexer repo
      run 8080 $ serve (Proxy :: Proxy Routes) (server env repo indexer)
  where
    env = WikiEnv { _envHost = "http://localhost:8080"
                  , _envRoot = "wiki"
                  , _envMainPage = "Hauptseite"
                  }
