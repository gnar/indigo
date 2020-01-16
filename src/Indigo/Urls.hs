module Indigo.Urls where

import Servant.Links (Link, linkURI)
import System.FilePath ((</>))
import Network.URI (URI(..), URIAuth(..))

import Indigo.Environment
import Control.Lens

buildURI :: env -> Link -> URI
buildURI env link = fixPath (linkURI link)
                      { uriScheme = "http:"
                      , uriAuthority = Just (URIAuth "" "localhost" ":8080")
                      }
  where
    fixPath uri@URI{uriPath = '/':_} = uri
    fixPath uri@URI{uriPath = path } = uri { uriPath = '/':path}

staticLink env path = "http://" <> env ^. envHost <> ":" <> show (env ^. envPort) <> "/static/" <> path
