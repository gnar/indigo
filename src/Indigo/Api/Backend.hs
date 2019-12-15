{-# LANGUAGE DataKinds #-}

module Indigo.Api.Backend (
  PageDto(..),
  BackendApi,
  PageApi
) where

import Servant
import Data.Aeson
import GHC.Generics
import Data.Text as T

data PageDto = PageDto { id :: Maybe Int
                       , name :: T.Text
                       , text :: T.Text
                       } deriving (Eq, Show, Generic, ToJSON, FromJSON)

type BackendApi = "pages" :> PageApi

type PageApi =
       Capture "id" Int :> Get '[JSON] PageDto
  :<|> Capture "id" Int :> ReqBody '[JSON] PageDto :> PostNoContent '[JSON] NoContent
  :<|> Capture "id" Int :> DeleteAccepted '[JSON] NoContent

  :<|> ReqBody '[JSON] PageDto :> PutCreated '[JSON] NoContent

  :<|> Capture "id" Int :> "contents" :> Get '[JSON] T.Text
  :<|> Capture "id" Int :> "contents" :> ReqBody '[JSON] T.Text :> PostNoContent '[JSON] NoContent
  :<|> Capture "id" Int :> "contents" :> "html" :> Get '[JSON] T.Text
