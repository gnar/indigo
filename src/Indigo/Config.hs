module Indigo.Config where

import Data.Aeson

jsonConfig = defaultOptions { fieldLabelModifier = \case
  ('_':str) -> str
  str -> str
}
