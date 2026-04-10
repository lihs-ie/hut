{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Server (API, api, server) where

import API.Handlers.Event (directEventHandler, pubsubHandler)
import API.Handlers.Health (healthHandler)
import API.Request (EventRequest, PubSubPushRequest)
import Servant

type API =
  "health" :> Get '[JSON] String
    :<|> "events" :> ReqBody '[JSON] PubSubPushRequest :> Post '[JSON] String
    :<|> "events" :> "direct" :> ReqBody '[JSON] EventRequest :> Post '[JSON] String

api :: Proxy API
api = Proxy

server :: Server API
server = healthHandler :<|> pubsubHandler :<|> directEventHandler
