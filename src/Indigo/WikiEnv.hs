module Indigo.WikiEnv (
    WikiEnv(..)
  , host
  , pageDir
  , staticDir
) where

import qualified Data.Text as T
import Control.Lens (Lens', lens)

import Indigo.Config

data WikiEnv = WikiEnv {
  _host :: T.Text,
  _pageDir :: FilePath,
  _staticDir :: FilePath
} deriving Show

host :: Lens' WikiEnv T.Text
host = lens _host $ \e h -> e { _host = h}

pageDir :: Lens' WikiEnv FilePath
pageDir = lens _pageDir $ \e pd -> e { _pageDir = pd}

staticDir :: Lens' WikiEnv FilePath
staticDir = lens _staticDir $ \e sd -> e { _staticDir = sd }
