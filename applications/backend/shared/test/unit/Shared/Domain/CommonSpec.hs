module Shared.Domain.CommonSpec (run) where

import Control.Exception (Exception)
import Shared.Domain.Error (
    AggregateNotFoundError (AggregateNotFoundError),
    DomainError (..),
    InvariantViolationError (InvariantViolationError),
    OperationNotAllowedError (OperationNotAllowedError),
    ServiceUnavailableError (ServiceUnavailableError),
    UnexpectedDomainError (UnexpectedDomainError),
    createAggregateNotFound,
    createInvariantViolation,
    createOperationNotAllowed,
    createServiceUnavailable,
    createUnexpectedError,
 )
import Shared.Domain.Event (DomainEvent (DomainEvent), Events (..), OneOf (Here, There))

data TestEventKind = TestEventKind

newtype FirstEvent = FirstEvent Int
    deriving stock (Show, Eq)

newtype SecondEvent = SecondEvent String
    deriving stock (Show, Eq)

run :: IO Bool
run =
    pure
        ( and
            [ domainEventRetainsPayload
            , emptyEventListIsRepresentable
            , firstTypedEventIsRepresentable
            , secondTypedEventIsRepresentable
            , domainErrorsRetainDetails
            , domainErrorDetailsAreExceptions
            ]
        )

domainEventRetainsPayload :: Bool
domainEventRetainsPayload =
    case DomainEvent 42 :: DomainEvent 'TestEventKind Int of
        DomainEvent payload -> payload == 42

emptyEventListIsRepresentable :: Bool
emptyEventListIsRepresentable =
    null (values (Events [] :: Events '[]))

firstTypedEventIsRepresentable :: Bool
firstTypedEventIsRepresentable =
    case Events [Here (FirstEvent 1)] :: Events '[FirstEvent, SecondEvent] of
        Events [Here value] -> value == FirstEvent 1
        Events _ -> False

secondTypedEventIsRepresentable :: Bool
secondTypedEventIsRepresentable =
    case Events [There (Here (SecondEvent "second"))] :: Events '[FirstEvent, SecondEvent] of
        Events [There (Here value)] -> value == SecondEvent "second"
        Events _ -> False

domainErrorsRetainDetails :: Bool
domainErrorsRetainDetails =
    and
        [ createInvariantViolation "Image" "invalid"
            == InvariantViolation (InvariantViolationError "Image" "invalid")
        , createAggregateNotFound "Image" "01ARZ3NDEKTSV4RRFFQ69G5FAV"
            == AggregateNotFound
                (AggregateNotFoundError "Image" "01ARZ3NDEKTSV4RRFFQ69G5FAV")
        , createOperationNotAllowed "Image" "already available"
            == OperationNotAllowed (OperationNotAllowedError "Image" "already available")
        , createServiceUnavailable "ImageStore" "timed out"
            == ServiceUnavailable (ServiceUnavailableError "ImageStore" "timed out")
        , createUnexpectedError "ImageUpload" "unexpected response"
            == UnexpectedError (UnexpectedDomainError "ImageUpload" "unexpected response")
        ]

domainErrorDetailsAreExceptions :: Bool
domainErrorDetailsAreExceptions =
    and
        [ hasExceptionInstance (InvariantViolationError "Image" "invalid")
        , hasExceptionInstance
            (AggregateNotFoundError "Image" "01ARZ3NDEKTSV4RRFFQ69G5FAV")
        , hasExceptionInstance (OperationNotAllowedError "Image" "already available")
        , hasExceptionInstance (ServiceUnavailableError "ImageStore" "timed out")
        , hasExceptionInstance (UnexpectedDomainError "ImageUpload" "unexpected response")
        ]

hasExceptionInstance :: (Exception error) => error -> Bool
hasExceptionInstance _ = True
