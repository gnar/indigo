{-# LANGUAGE DataKinds #-}

module Indigo.Api.Frontend (
  PagesUi(..),
  FrontendUi(..),
) where

import Data.Aeson
import GHC.Generics
import Data.Text as T

import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Markup)

type PagesUi = "pages" :> Capture "page" T.Text :> Get '[HTML] Markup
type FrontendUi = PagesUi
