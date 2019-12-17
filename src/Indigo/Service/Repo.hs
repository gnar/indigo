module Indigo.Service.Repo where

import Data.Maybe (maybe)
import qualified Data.Text as T

import Indigo.Page

data Handle = Handle {
    pageIndex :: IO [T.Text]
  , loadPage :: T.Text -> IO (Maybe Page)
  , updatePage :: Page -> IO Page
  , deletePage :: T.Text -> IO ()
}

loadOrCreatePage :: Handle -> T.Text -> IO Page
loadOrCreatePage repo name = loadPage repo name >>= maybe (updatePage repo $ newPage name) pure
