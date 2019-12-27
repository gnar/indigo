{-# LANGUAGE TemplateHaskell #-}
module Indigo.Resources
  ( staticFiles
  ) where

import Data.FileEmbed

import qualified Data.ByteString as B

staticFiles :: [(FilePath, B.ByteString)]
staticFiles = $(embedDir "static/")
