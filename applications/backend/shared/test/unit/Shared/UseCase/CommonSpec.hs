module Shared.UseCase.CommonSpec (run) where

import Data.Either (isLeft)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Shared.Domain.Error (createInvariantViolation)
import Shared.Domain.Event (DomainEvent (DomainEvent))
import Shared.UseCase.Command (Command (Command))
import Shared.UseCase.Context (
    actorText,
    causationText,
    correlationIdentifierText,
    newActor,
    newCausation,
    newCorrelationIdentifier,
 )
import Shared.UseCase.Event (EventEnvelope (EventEnvelope), newEventEnvelope, newEventIdentifier)

data TestEventKind = TestEventKind

run :: IO Bool
run =
    pure
        ( and
            [ isLeft (newActor "")
            , isLeft (newCorrelationIdentifier "  ")
            , isLeft (newCorrelationIdentifier "01arz3ndektsv4rrffq69g5fav")
            , isLeft (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAI")
            , isLeft (newCorrelationIdentifier "8ZZZZZZZZZZZZZZZZZZZZZZZZZ")
            , correlationFailureIsDomainError
            , isLeft (newCausation "")
            , contextValuesRoundTrip
            , commandHasCommonShape
            , envelopeHasDeliveryMetadata
            , isLeft (newEventIdentifier "")
            ]
        )

commandHasCommonShape :: Bool
commandHasCommonShape =
    case ( newActor "administrator"
         , newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV"
         , newCausation "cause"
         ) of
        (Right actor, Right correlation, Right causation) ->
            case Command ("payload" :: String) baseTime actor correlation (Just causation) of
                Command payload timestamp _ _ (Just _) ->
                    payload == "payload" && timestamp == baseTime
        _ -> False

envelopeHasDeliveryMetadata :: Bool
envelopeHasDeliveryMetadata =
    case ( newEventIdentifier "event-1"
         , newActor "administrator"
         , newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV"
         , newCausation "cause"
         ) of
        (Right eventIdentifier, Right actor, Right correlation, Right causation) ->
            case newEventEnvelope
                eventIdentifier
                baseTime
                actor
                correlation
                (Just causation)
                (DomainEvent 42 :: DomainEvent 'TestEventKind Int) of
                EventEnvelope
                    identifier
                    occurredAt
                    envelopeActor
                    envelopeCorrelation
                    envelopeCausation
                    (DomainEvent payload) ->
                        identifier == eventIdentifier
                            && occurredAt == baseTime
                            && envelopeActor == actor
                            && envelopeCorrelation == correlation
                            && envelopeCausation == Just causation
                            && payload == 42
        _ -> False

contextValuesRoundTrip :: Bool
contextValuesRoundTrip =
    case ( newActor "administrator"
         , newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV"
         , newCausation "cause"
         ) of
        (Right actor, Right correlation, Right causation) ->
            actorText actor == "administrator"
                && correlationIdentifierText correlation == "01ARZ3NDEKTSV4RRFFQ69G5FAV"
                && causationText causation == "cause"
        _ -> False

correlationFailureIsDomainError :: Bool
correlationFailureIsDomainError =
    newCorrelationIdentifier "invalid"
        == Left
            ( createInvariantViolation
                "CorrelationIdentifier"
                "value must be a canonical ULID"
            )

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2026 9 12) 0
