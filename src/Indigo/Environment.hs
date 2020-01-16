module Indigo.Environment (
    Environment(..)
  , envHost
  , envPort
  , envStore
  , envMainPage
) where

import Control.Lens ((^.), Lens', lens)
import qualified Data.Text as T

import qualified Indigo.Api as Api


data Environment = Environment
  { _envHost :: String
  , _envPort :: Integer
  , _envStore :: FilePath
  , _envMainPage :: T.Text
  } deriving Show

envHost :: Lens' Environment String
envHost = lens _envHost $ \e h -> e { _envHost = h }

envPort :: Lens' Environment Integer
envPort = lens _envPort $ \e p -> e { _envPort = p }

envStore :: Lens' Environment FilePath
envStore = lens _envStore $ \e r -> e { _envStore = r }

envMainPage :: Lens' Environment T.Text
envMainPage = lens _envMainPage $ \e h -> e { _envMainPage = h }
