module Shared.UseCase.Command (
    Command (..),
    Actor,
    CorrelationIdentifier,
    Causation,
    ContextValueError,
    CommandValueError,
    newActor,
    actorText,
    newCorrelationIdentifier,
    correlationIdentifierText,
    newCausation,
    causationText,
) where

import Data.Time (UTCTime)
import Shared.UseCase.Context (
    Actor,
    Causation,
    ContextValueError,
    CorrelationIdentifier,
    actorText,
    causationText,
    correlationIdentifierText,
    newActor,
    newCausation,
    newCorrelationIdentifier,
 )

type CommandValueError = ContextValueError

data Command payload = Command
    { payload :: payload
    , timestamp :: UTCTime
    , actor :: Actor
    , correlation :: CorrelationIdentifier
    , causation :: Maybe Causation
    }
    deriving stock (Show, Eq)
