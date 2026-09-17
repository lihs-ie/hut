module Shared.UseCase.Context (
    Actor,
    CorrelationIdentifier,
    Causation,
    ContextValueError,
    newActor,
    actorText,
    newCorrelationIdentifier,
    correlationIdentifierText,
    newCausation,
    causationText,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Shared.Domain.Error (DomainError, createInvariantViolation)
import Shared.Domain.Identifier (ULID, newULID, ulidText)

newtype Actor = Actor Text
    deriving stock (Show, Eq)

newtype CorrelationIdentifier = CorrelationIdentifier ULID
    deriving stock (Show, Eq)

newtype Causation = Causation Text
    deriving stock (Show, Eq)

type ContextValueError = DomainError

newActor :: Text -> Either ContextValueError Actor
newActor = newNonEmptyText Actor "Actor"

actorText :: Actor -> Text
actorText (Actor value) = value

newCorrelationIdentifier :: Text -> Either ContextValueError CorrelationIdentifier
newCorrelationIdentifier value =
    case newULID value of
        Right ulid -> Right (CorrelationIdentifier ulid)
        Left _ -> Left correlationIdentifierError

correlationIdentifierText :: CorrelationIdentifier -> Text
correlationIdentifierText (CorrelationIdentifier value) = ulidText value

newCausation :: Text -> Either ContextValueError Causation
newCausation = newNonEmptyText Causation "Causation"

causationText :: Causation -> Text
causationText (Causation value) = value

newNonEmptyText :: (Text -> value) -> Text -> Text -> Either ContextValueError value
newNonEmptyText constructor valueName value
    | Text.null (Text.strip value) =
        Left (createInvariantViolation valueName "value must not be empty")
    | otherwise = Right (constructor value)

correlationIdentifierError :: ContextValueError
correlationIdentifierError =
    createInvariantViolation
        "CorrelationIdentifier"
        "value must be a canonical ULID"
