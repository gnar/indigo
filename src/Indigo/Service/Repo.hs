module Indigo.Service.Repo where

import Data.Maybe (maybe)
import qualified Data.Text as T

import Indigo.Doc

data Handle = Handle {
    listDocs :: IO [DocName]
  , loadMeta :: DocName -> IO (Maybe DocMeta)
  , loadDoc :: DocName -> IO (Maybe Doc)
  , saveDoc :: Doc -> IO Doc
  , deleteDoc :: DocName -> IO ()
}

loadOrCreateDoc :: Handle -> DocName -> IO Doc
loadOrCreateDoc repo name = loadDoc repo name >>= maybe (saveDoc repo $ newDocPage name) pure
