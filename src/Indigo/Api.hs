module Indigo.Api (
    module Indigo.Api.Frontend
  , module Indigo.Api.Backend
) where

import Indigo.Api.Frontend
import Indigo.Api.Backend

import Servant (Proxy(..), allLinks)

backendApi = Proxy :: Proxy BackendApi
frontEndUi = Proxy :: Proxy FrontendUi

-- articleLink = allLinks frontendUi
