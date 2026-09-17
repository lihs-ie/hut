module Shared.Domain.Error (
    DomainError (..),
    InvariantViolationError (..),
    AggregateNotFoundError (..),
    OperationNotAllowedError (..),
    ServiceUnavailableError (..),
    UnexpectedDomainError (..),
    createInvariantViolation,
    createAggregateNotFound,
    createOperationNotAllowed,
    createServiceUnavailable,
    createUnexpectedError,
) where

import Control.Exception (Exception)
import Data.Text (Text)

data DomainError
    = InvariantViolation InvariantViolationError
    | AggregateNotFound AggregateNotFoundError
    | OperationNotAllowed OperationNotAllowedError
    | ServiceUnavailable ServiceUnavailableError
    | UnexpectedError UnexpectedDomainError
    deriving stock (Show, Eq)

instance Exception DomainError

data InvariantViolationError = InvariantViolationError
    { name :: Text
    , reason :: Text
    }
    deriving stock (Show, Eq)

createInvariantViolation :: Text -> Text -> DomainError
createInvariantViolation name reason = InvariantViolation (InvariantViolationError name reason)

instance Exception InvariantViolationError

data AggregateNotFoundError = AggregateNotFoundError
    { name :: Text
    , identifier :: Text
    }
    deriving stock (Show, Eq)

instance Exception AggregateNotFoundError

createAggregateNotFound :: Text -> Text -> DomainError
createAggregateNotFound name identifier = AggregateNotFound (AggregateNotFoundError name identifier)

data OperationNotAllowedError = OperationNotAllowedError
    { name :: Text
    , reason :: Text
    }
    deriving stock (Show, Eq)

instance Exception OperationNotAllowedError

createOperationNotAllowed :: Text -> Text -> DomainError
createOperationNotAllowed name reason =
    OperationNotAllowed (OperationNotAllowedError name reason)

data ServiceUnavailableError = ServiceUnavailableError
    { name :: Text
    , reason :: Text
    }
    deriving stock (Show, Eq)

instance Exception ServiceUnavailableError

createServiceUnavailable :: Text -> Text -> DomainError
createServiceUnavailable name reason =
    ServiceUnavailable (ServiceUnavailableError name reason)

data UnexpectedDomainError = UnexpectedDomainError
    { name :: Text
    , reason :: Text
    }
    deriving stock (Show, Eq)

instance Exception UnexpectedDomainError

createUnexpectedError :: Text -> Text -> DomainError
createUnexpectedError name reason =
    UnexpectedError (UnexpectedDomainError name reason)
