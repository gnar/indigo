{-# LANGUAGE DataKinds #-}

module Indigo.Api.Frontend (
  PagesUi(..),
  FrontendUi(..),
  PageAction(..)
) where

import Data.Aeson
import GHC.Generics
import Data.Text as T

import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Markup)

data PageAction = PageView | PageEdit | PageDelete | PageCreate deriving (Eq, Show)

instance FromHttpApiData PageAction where
  parseQueryParam "create" = Right PageCreate
  parseQueryParam "view" = Right PageView
  parseQueryParam "edit" = Right PageEdit
  parseQueryParam "delete" = Right PageDelete

type PagesUi = "pages" :> Capture "page" T.Text :> QueryParam "action" PageAction :> Get '[HTML] Markup
type FrontendUi = PagesUi
