{-# LANGUAGE RecordWildCards #-}
module Indigo.Render ( renderPage ) where

import Servant
import Control.Lens

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as A
--import qualified Text.Blaze.Internal as H (attribute)

import qualified Data.Text as T

import Data.Aeson (toJSON, encode)
import Data.Aeson

import Data.Pool
import Database.PostgreSQL.Simple

import qualified Indigo.Api as Api
import Indigo.Page
import Indigo.WikiEnv
import Indigo.WikiTag

import Servant.Links

import Data.Maybe (fromJust, fromMaybe)
import Data.String
import Data.String.QQ
import Data.ByteString

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO)

linkPrefix :: (IsString a) => a
linkPrefix = "http://localhost:8080/"

renderLink :: Link -> T.Text
renderLink link = T.append linkPrefix $ toUrlPiece link

renderPageTemplate :: T.Text -> H.Html -> H.Html
renderPageTemplate title contents =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.meta ! A.name "description" ! A.content ""
      H.meta ! A.name "author" ! A.content ""
      H.link ! A.rel "stylesheet" ! A.href (H.toValue (linkPrefix ++ "static/css/bootstrap.min.css"))
      H.title $ H.toHtml title
    H.body $ do
      H.header $ do
        H.nav ! A.class_ "navbar navbar-expand-lg navbar-light bg-light" $ do
          H.div ! A.class_ "collapse navbar-collapse" ! A.id "navbarSupportedContent" $ do
              H.ul ! A.class_ "navbar-nav mr-auto" $ do
                  H.li ! A.class_ "nav-item" $ H.a ! A.class_ "nav-link" ! A.href "Hauptseite" $ "Home"
                  H.li ! A.class_ "nav-item dropdown" $ do
                      H.a ! A.class_ "nav-link dropdown-toggle" ! A.href "#" ! A.id "navbarDropdown" ! H.dataAttribute "toggle" "dropdown" $ "This page"
                      H.div ! A.class_ "dropdown-menu" $ do
                          H.a ! A.class_ "dropdown-item" ! A.href "?action=edit" $ "Edit"
                          H.a ! A.class_ "dropdown-item" ! A.href "?action=delete" $ "Delete"
              H.form ! A.class_ "form-inline my-2 my-lg-0" $ do
                  H.input ! A.class_ "form-control mr-sm-2" ! A.type_ "search" ! A.placeholder "Search"
                  H.button ! A.class_ "btn btn-outline-success my-2 my-sm-0" ! A.type_ "submit" $ "Search"
      H.div ! A.class_ "container" $ do
        contents
        H.script ! A.src (H.toValue (linkPrefix ++ "static/js/jquery-3.4.1.min.js")) $ pure ()
        H.script ! A.src (H.toValue (linkPrefix ++ "static/js/bootstrap.min.js")) $ pure ()
        H.script ! A.src (H.toValue (linkPrefix ++ "static/api.js")) $ pure ()

renderPage :: WikiEnv -> Page -> H.Html
renderPage env page = do
  renderPageTemplate (page ^. name) $ do
    H.span ! A.id "rendered-article-contents" $ pageHtml env page
