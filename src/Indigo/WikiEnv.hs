module Indigo.WikiEnv (
    WikiEnv(..)
  , envHost
  , envPageDir
  , envStaticDir
  , envMainPage
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
import Network.URI (uriToString)
import System.FilePath ((</>))

data WikiEnv = WikiEnv
  { _envHost :: T.Text
  , _envRoot :: FilePath
  , _envMainPage :: T.Text
  } deriving Show

envHost :: Lens' WikiEnv T.Text
envHost = lens _envHost $ \e h -> e { _envHost = h}

envRoot :: Lens' WikiEnv FilePath
envRoot = lens _envRoot $ \e r -> e { _envRoot = r }

envMainPage :: Lens' WikiEnv T.Text
envMainPage = lens _envMainPage $ \e h -> e { _envMainPage = h }

envPageDir :: WikiEnv -> FilePath
envPageDir env = (env ^. envRoot) </> "pages"

envStaticDir :: WikiEnv -> FilePath
envStaticDir env = (env ^. envRoot) </> "static"

linkUrl :: WikiEnv -> Link -> T.Text
linkUrl env link = env ^. envHost <> "/" <> T.pack (uriToString id (linkURI link) "")

pageUrl     env name        = linkUrl env $ Api.linkGetPage name Nothing
pageUrl'    env name action = linkUrl env $ Api.linkGetPage name (Just action)
pageFileUrl env name file   = linkUrl env $ Api.linkGetPageFile name file
pagesUrl    env             = linkUrl env   Api.linkGetPages
tagUrl      env tag         = linkUrl env $ Api.linkGetTag tag
tagsUrl     env             = linkUrl env   Api.linkGetTags
