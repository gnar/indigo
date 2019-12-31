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
import           Text.Blaze.Html               (Html, ToMarkup, toHtml)

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Text.Blaze.Html5 (Markup, toHtml)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy as LB

import Indigo.Environment
import Indigo.Api as Api
import Indigo.Page as Page
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

renderEditNewPage env name =
    renderEditPage env (newPage, newText)
  where
    (newPage, _, newText) = Ops.newPage name

frontend :: Environment -> Repo.Handle -> Indexer.Handle -> Server FrontendApi
frontend env repo indexer = listPages :<|> getPage :<|> postPage :<|> repoFile :<|> listTags :<|> getTag :<|> hmm
  where
    redirectToDoc :: T.Text -> Handler a
    redirectToDoc name = throwError err303 { errHeaders = [(HTTP.hLocation, T.encodeUtf8 $ pageUrl env name) ] }

    listPages  :: Handler Markup
    listPages  = liftIO $ renderListPages env <$> Indexer.findAllPages indexer

    name2page = T.pack . dropExtension

    getPage :: FilePath -> Maybe PageAction -> Handler Markup
    getPage path (Just PageView) | name <- name2page path = liftIO $ Ops.loadPage repo name <&> maybe (renderEditNewPage env name) (\(a, b, _) -> renderViewPage env (a, b))
    getPage path (Just PageEdit) | name <- name2page path  = liftIO $ Ops.loadPage repo name <&> maybe (renderEditNewPage env name) (\(a, _, b) -> renderEditPage env (a, b))
    getPage path (Just PageDelete) | name <- name2page path  = liftIO (Ops.deletePage repo name) >> redirectToDoc name
    getPage path _ = getPage path (Just PageView)

    postPage :: FilePath -> PageForm -> Handler Markup
    postPage path form | name <- name2page path = do
      liftIO $ do
        (page, _, _) <- Ops.savePage repo name (Api.text form)
        Indexer.update indexer page
      redirectToDoc name
      where
        parseTags = filter (not . T.null) . fmap T.strip . T.splitOn ","

    repoFile :: FilePath -> Handler (Headers '[Header "Content-Type" String, Header "Content-Disposition" String] LB.ByteString)
    repoFile path =
      liftIO $ do
        stream <- LB.fromStrict . fromJust <$> Repo.loadFile repo path
        let contentType = guessMimeType path
            contentDisposition = "filename=" <> path
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

server :: Environment -> Repo.Handle -> Indexer.Handle -> Server Routes
server env repo indexer = frontend env repo indexer :<|> serveDirectoryEmbedded staticFiles

runServer :: Environment -> IO ()
runServer env =
  Repo.withHandle (env ^. envStore) $ \repo ->
    Indexer.withHandle $ \indexer -> do
      Indexer.rebuild indexer repo
      run 8080 $ serve (Proxy :: Proxy Routes) (server env repo indexer)
