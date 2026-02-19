module API.Handlers.Health (healthHandler) where

import Servant (Handler)

healthHandler :: Handler String
healthHandler = pure "ok"
