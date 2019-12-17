module Indigo.Service.Repo where

import Indigo.Page
import qualified Data.Text as T

import Control.Monad.IO.Class
import Data.Maybe (maybe)

data Handle = Handle {
    loadPage :: T.Text -> IO (Maybe Page)
  , updatePage :: Page -> IO Page
  , deletePage :: T.Text -> IO ()
}

loadOrCreatePage :: Handle -> T.Text -> IO Page
loadOrCreatePage repo name = loadPage repo name >>= maybe (updatePage repo $ newPage name) pure
