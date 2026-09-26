module Shared.UseCase.Event (
    EventIdentifier,
    EventIdentifierError (..),
    EventEnvelope (..),
    newEventIdentifier,
    eventIdentifierText,
    newEventEnvelope,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Shared.UseCase.Context (Actor, Causation, CorrelationIdentifier)

newtype EventIdentifier = EventIdentifier Text
    deriving stock (Show, Eq)

data EventIdentifierError = EventIdentifierError
    { reason :: Text
    }
    deriving stock (Show, Eq)

data EventEnvelope event = EventEnvelope
    { identifier :: EventIdentifier
    , occurredAt :: UTCTime
    , actor :: Actor
    , correlation :: CorrelationIdentifier
    , causation :: Maybe Causation
    , event :: event
    }
    deriving stock (Show, Eq)

newEventIdentifier :: Text -> Either EventIdentifierError EventIdentifier
newEventIdentifier value
    | Text.null (Text.strip value) = Left (EventIdentifierError "value must not be empty")
    | otherwise = Right (EventIdentifier value)

eventIdentifierText :: EventIdentifier -> Text
eventIdentifierText (EventIdentifier value) = value

newEventEnvelope ::
    EventIdentifier ->
    UTCTime ->
    Actor ->
    CorrelationIdentifier ->
    Maybe Causation ->
    event ->
    EventEnvelope event
newEventEnvelope = EventEnvelope
