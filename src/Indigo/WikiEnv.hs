module Indigo.WikiEnv (
    WikiEnv(..)
  , host
  , pageDir
  , staticDir
  , mainPage
  , pageUrl
  , pageUrl'
  , pageFileUrl
  , pagesUrl
  , tagUrl
  , tagsUrl
) where

import Control.Lens ((^.), Lens', lens)
import qualified Data.Text as T

import Indigo.Config
import qualified Indigo.Api as Api
import Servant.Links (Link, linkURI)
import Network.URI (URI, uriToString, uriIsRelative, uriIsAbsolute)

data WikiEnv = WikiEnv
  { _host :: T.Text
  , _pageDir :: FilePath
  , _staticDir :: FilePath
  , _mainPage :: T.Text
  } deriving Show

host :: Lens' WikiEnv T.Text
host = lens _host $ \e h -> e { _host = h}

pageDir :: Lens' WikiEnv FilePath
pageDir = lens _pageDir $ \e pd -> e { _pageDir = pd}

staticDir :: Lens' WikiEnv FilePath
staticDir = lens _staticDir $ \e sd -> e { _staticDir = sd }

mainPage :: Lens' WikiEnv T.Text
mainPage = lens _mainPage $ \e h -> e { _mainPage = h}

linkUrl :: WikiEnv -> Link -> T.Text
linkUrl env link = env ^. host <> "/" <> T.pack (uriToString id (linkURI link) "")

pageUrl     env name        = linkUrl env $ Api.linkGetPage name Nothing
pageUrl'    env name action = linkUrl env $ Api.linkGetPage name (Just action)
pageFileUrl env name file   = linkUrl env $ Api.linkGetPageFile name file
pagesUrl    env             = linkUrl env   Api.linkGetPages
tagUrl      env tag         = linkUrl env $ Api.linkGetTag tag
tagsUrl     env             = linkUrl env   Api.linkGetTags
