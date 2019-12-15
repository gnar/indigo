module Indigo.WikiEnv (
    WikiEnv(..)
  , host
  , pageDir
  , staticDir
  , jsonConfig
) where

import qualified Data.Text as T
import Control.Lens
import Data.Aeson
import GHC.Generics (Generic)

data WikiEnv = WikiEnv {
  _host :: T.Text,
  _pageDir :: T.Text,
  _staticDir :: T.Text
} deriving (Eq, Show, Generic)

instance ToJSON WikiEnv where toJSON = genericToJSON jsonConfig
instance FromJSON WikiEnv where parseJSON = genericParseJSON jsonConfig

host :: Lens' WikiEnv T.Text
host = lens _host $ \e h -> e { _host = h}

pageDir :: Lens' WikiEnv T.Text
pageDir = lens _pageDir $ \e pd -> e { _pageDir = pd}

staticDir :: Lens' WikiEnv T.Text
staticDir = lens _staticDir $ \e sd -> e { _staticDir = sd }

jsonConfig = defaultOptions { fieldLabelModifier = \case
  ('_':str) -> str
  str -> str
}
