module Indigo.Service.Repo where

import Data.Maybe (maybe)
import qualified Data.Text as T

import Indigo.Page

data Handle = Handle {
    listDocs :: IO [DocName]
  , loadMeta :: DocName -> IO (Maybe DocMeta)
  , loadDoc :: DocName -> IO (Maybe Document)
  , saveDoc :: Document -> IO Document
  , deleteDoc :: DocName -> IO ()
}

loadOrCreateDoc :: Handle -> DocName -> IO Document
loadOrCreateDoc repo name = loadDoc repo name >>= maybe (saveDoc repo $ newPage name) pure
