{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Server (API, api, server) where

import API.Handlers.Event (directEventHandler, pubsubHandler)
import API.Handlers.Health (healthHandler)
import API.Request (EventRequest, PubSubPushRequest)
import Aspects.Log (LogEntry)
import Control.Monad.Writer (WriterT)
import Domain.SearchToken (Persist, TerminateByReference)
import Servant

type API =
  "health" :> Get '[JSON] String
    :<|> "events" :> ReqBody '[JSON] PubSubPushRequest :> Post '[JSON] String
    :<|> "events" :> "direct" :> ReqBody '[JSON] EventRequest :> Post '[JSON] String

api :: Proxy API
api = Proxy

server :: Persist (WriterT [LogEntry] IO) -> TerminateByReference (WriterT [LogEntry] IO) -> Server API
server persist terminate =
  healthHandler :<|> pubsubHandler persist terminate :<|> directEventHandler persist terminate
