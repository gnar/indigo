module Indigo.Environment (
    Environment(..)
  , envHost
  , envStore
  , envMainPage
  , pageUrl
  , pageUrl'
  , repoFileUrl
  , rootUrl
  , tagUrl
  , tagsUrl
) where

import Control.Lens ((^.), Lens', lens)
import qualified Data.Text as T

import qualified Indigo.Api as Api
import Servant.Links (Link, linkURI)
import Network.URI (uriToString)
import System.FilePath ((</>))

data Environment = Environment
  { _envHost :: T.Text
  , _envStore :: FilePath
  , _envMainPage :: T.Text
  } deriving Show

envHost :: Lens' Environment T.Text
envHost = lens _envHost $ \e h -> e { _envHost = h}

envStore :: Lens' Environment FilePath
envStore = lens _envStore $ \e r -> e { _envStore = r }

envMainPage :: Lens' Environment T.Text
envMainPage = lens _envMainPage $ \e h -> e { _envMainPage = h }

linkUrl :: Environment -> Link -> T.Text
linkUrl env link = env ^. envHost <> "/" <> T.pack (uriToString id (linkURI link) "")

rootUrl     env             = linkUrl env   Api.linkRoot
pageUrl     env name        = linkUrl env $ Api.linkPage name Nothing
pageUrl'    env name action = linkUrl env $ Api.linkPage name (Just action)
repoFileUrl env path        = linkUrl env $ Api.linkRepoFile path
tagUrl      env tag         = linkUrl env $ Api.linkTag tag
tagsUrl     env             = linkUrl env   Api.linkTags
