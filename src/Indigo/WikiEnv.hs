module Indigo.WikiEnv where

import qualified Data.Text as T
import Control.Lens

data WikiEnv = WikiEnv {
  _host :: T.Text,
  _pageDir :: T.Text
} deriving (Eq, Show)

host :: Lens' WikiEnv T.Text
host = lens _host (\e h -> e { _host = h})

pageDir :: Lens' WikiEnv T.Text
pageDir = lens _pageDir (\e pd -> e { _pageDir = pd})

env0 = WikiEnv {
  _host = "http://localhost:8080/pages",
  _pageDir = "pages"
}
