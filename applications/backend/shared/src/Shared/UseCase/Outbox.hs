module Shared.UseCase.Outbox (Append) where

import Shared.Domain.Event (Events)
import Shared.UseCase.Command (Command)

-- Persist enveloped events atomically with aggregate changes; never publish here.
type Append events m = Command () -> Events events -> m ()
