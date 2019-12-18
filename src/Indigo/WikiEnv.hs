module Indigo.WikiEnv (
    WikiEnv(..)
  , host
  , pageDir
  , staticDir
  , mainPage
) where

import qualified Data.Text as T
import Control.Lens (Lens', lens)

import Indigo.Config

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
