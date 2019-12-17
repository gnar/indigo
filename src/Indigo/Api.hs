module Indigo.Api (
  PageAction(..),
  PageForm(..),
  FrontendApi
) where

import Data.Aeson
import GHC.Generics
import Data.Text as T

import Servant
import Servant.HTML.Blaze (HTML)
import Web.FormUrlEncoded (FromForm)
import Text.Blaze.Html (Html)

data PageAction = PageView | PageEdit | PageDelete | PageCreate deriving (Eq, Show)

instance FromHttpApiData PageAction where
  parseQueryParam "create" = Right PageCreate
  parseQueryParam "view" = Right PageView
  parseQueryParam "edit" = Right PageEdit
  parseQueryParam "delete" = Right PageDelete

data PageForm = PageForm { text :: T.Text
                         , name :: T.Text } deriving (Eq, Show, Generic, FromForm)

type FrontendApi = "pages" :> Capture "page" T.Text :> QueryParam "action" PageAction     :>  Get '[HTML] Html
              :<|> "pages" :> Capture "page" T.Text :> ReqBody '[FormUrlEncoded] PageForm :> Post '[HTML] Html
