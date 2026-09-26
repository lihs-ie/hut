module Shared.Domain.Error (
    DomainError (..),
    ProcessingTargetChangedError (..),
    createProcessingTargetChanged,
    InvariantViolationError (..),
    AggregateNotFoundError (..),
    OperationNotAllowedError (..),
    ServiceUnavailableError (..),
    UnexpectedDomainError (..),
    TransactionOutcomeUnknownError (..),
    createInvariantViolation,
    createAggregateNotFound,
    createOperationNotAllowed,
    createServiceUnavailable,
    createUnexpectedError,
    createTransactionOutcomeUnknown,
) where

import Control.Exception (Exception)
import Data.Text (Text)

data DomainError
    = InvariantViolation InvariantViolationError
    | AggregateNotFound AggregateNotFoundError
    | OperationNotAllowed OperationNotAllowedError
    | ServiceUnavailable ServiceUnavailableError
    | UnexpectedError UnexpectedDomainError
    | TransactionOutcomeUnknown TransactionOutcomeUnknownError
    | ProcessingTargetChanged ProcessingTargetChangedError
    deriving stock (Show, Eq)

instance Exception DomainError

data ProcessingTargetChangedError = ProcessingTargetChangedError
    { name :: Text
    , reason :: Text
    }
    deriving stock (Show, Eq)

instance Exception ProcessingTargetChangedError

createProcessingTargetChanged :: Text -> Text -> DomainError
createProcessingTargetChanged name reason =
    ProcessingTargetChanged (ProcessingTargetChangedError name reason)

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

data TransactionOutcomeUnknownError = TransactionOutcomeUnknownError
    { name :: Text
    , reason :: Text
    }
    deriving stock (Show, Eq)

instance Exception TransactionOutcomeUnknownError

createTransactionOutcomeUnknown :: Text -> Text -> DomainError
createTransactionOutcomeUnknown name reason =
    TransactionOutcomeUnknown (TransactionOutcomeUnknownError name reason)
