module Indigo.Api
  ( PageAction(..)
  , PageForm(..)
  , FrontendApi

  , linkGetPages
  , linkGetPage
  , linkGetPageFile
  , linkGetTags
  , linkGetTag
) where

import Data.Aeson
import GHC.Generics
import Data.Text as T

import Servant
import Servant.Multipart
import Servant.HTML.Blaze (HTML)
import Web.FormUrlEncoded (FromForm)
import Text.Blaze.Html (Html)

import qualified Data.ByteString.Lazy as BS

data PageAction = PageView | PageEdit | PageDelete | PageCreate deriving (Eq, Show)

instance FromHttpApiData PageAction where
  parseQueryParam "create" = Right PageCreate
  parseQueryParam "view" = Right PageView
  parseQueryParam "edit" = Right PageEdit
  parseQueryParam "delete" = Right PageDelete

instance ToHttpApiData PageAction where
  toQueryParam PageCreate = "create"
  toQueryParam PageView = "view"
  toQueryParam PageEdit = "edit"
  toQueryParam PageDelete = "delete"

data PageForm = PageForm { text :: T.Text
                         , name :: T.Text
                         , tags :: T.Text } deriving (Eq, Show, Generic, FromForm)

type FrontendApi = "docs" :> Get '[HTML] Html
              :<|> "docs" :> Capture "page" T.Text :> QueryParam "action" PageAction     :>  Get '[HTML] Html
              :<|> "docs" :> Capture "page" T.Text :> ReqBody '[FormUrlEncoded] PageForm :> Post '[HTML] Html
              :<|> "docs" :> Capture "name" T.Text :> Capture "file" T.Text :> Get '[OctetStream] (Headers '[Header "Content-Type" String, Header "Content-Disposition" String] BS.ByteString)

              :<|> "tags" :> Get '[HTML] Html
              :<|> "tags" :> Capture "tag" T.Text :>  Get '[HTML] Html

              :<|> "hmm" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] Html

linkGetPages :<|> linkGetPage :<|> _ :<|> linkGetPageFile :<|> linkGetTags :<|> linkGetTag :<|> _ = allLinks (Proxy :: Proxy FrontendApi)
