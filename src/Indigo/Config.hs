module Indigo.Config where

import Data.Aeson
import qualified Data.Text as T
import System.FilePath (takeExtension)

jsonConfig = defaultOptions { fieldLabelModifier = \case
  ('_':str) -> str
  str -> str
}

guessMimeType :: FilePath -> String
guessMimeType f
  | ext `elem` [".jpg", "*.jpeg"] = "image/jpeg"
  | ext == ".png" = "image/png"
  | ext == ".md" = "text/markdown"
  | otherwise = "data/octet-stream"
  where
    ext = toLower (takeExtension f)
    toLower = T.unpack . T.toLower . T.pack
