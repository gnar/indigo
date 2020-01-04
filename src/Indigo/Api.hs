{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Indigo.Api
  ( PageAction(..)
  , PageForm(..)
  , FrontendApi
  , HTML
  , buildRootLink
  , buildPageLink
  , buildTagLink
  , buildTagsLink
  , buildFileLink
) where

import           GHC.Generics (Generic)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty            (NonEmpty(..))
import           Data.Typeable                 (Typeable)

import           Servant
import           Servant.API                   (Accept (..), MimeRender (..))
import           Servant.Multipart
import           Web.FormUrlEncoded (FromForm)

import           Text.Blaze.Html               (Html, ToMarkup, toHtml)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

data HTML deriving Typeable
data Markdown deriving Typeable

instance Accept HTML where
  contentTypes _ = "text/html; charset=utf-8" :| ["text/html"]

instance ToMarkup a => MimeRender HTML a where
  mimeRender _ = renderHtml . toHtml

instance Accept Markdown where
  contentTypes _ = "text/markdown; charset=utf-8" :| ["text/markdown"]

instance MimeRender Markdown T.Text where
  mimeRender _ = BL.fromStrict . T.encodeUtf8

-----

data PageAction = PageView | PageEdit | PageDelete  deriving (Eq, Show)

instance FromHttpApiData PageAction where
  parseQueryParam "view" = Right PageView
  parseQueryParam "edit" = Right PageEdit
  parseQueryParam "delete" = Right PageDelete

instance ToHttpApiData PageAction where
  toQueryParam PageView = "view"
  toQueryParam PageEdit = "edit"
  toQueryParam PageDelete = "delete"

data PageForm = PageForm { text :: T.Text
                         , name :: T.Text } deriving (Eq, Show, Generic, FromForm)

type FrontendApi = "raw" :> Capture "file" FilePath :> Get '[OctetStream] (Headers '[Header "Content-Type" String, Header "Content-Disposition" String] BL.ByteString)
              :<|> "tag" :> Get '[HTML] Html
              :<|> "tag" :> Capture "tag" T.Text :> Get '[HTML] Html
              :<|> "hmm" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] Html

              :<|> Get '[HTML] Html
              :<|> Capture "path" T.Text :> QueryParam "action" PageAction     :>  Get '[HTML] Html
              :<|> Capture "path" T.Text :> ReqBody '[FormUrlEncoded] PageForm :> Post '[HTML] Html

buildFileLink :<|> buildTagsLink :<|> buildTagLink :<|> _ :<|> buildRootLink :<|> buildPageLink :<|> _ = allLinks (Proxy :: Proxy FrontendApi)
