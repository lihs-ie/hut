{-# LANGUAGE RankNTypes #-}

module Shared.Infrastructure.Transaction (
    TransactionOutcome (..),
    TransactionDriver (..),
    transactionAction,
    newTransactionManager,
) where

import Data.Text (Text)
import Shared.Domain.Common.Transaction.Internal
import Shared.Domain.Error (DomainError, createTransactionOutcomeUnknown)

data TransactionOutcome a
    = Committed a
    | RolledBack DomainError
    | OutcomeUnknown Text

-- Invoke once inside the physical transaction; Left MUST roll back.
-- Each run gets a fresh context. Report only confirmed commit/rollback.
-- The driver owns exception/cancellation cleanup and rejects reentry.
newtype TransactionDriver context m = TransactionDriver
    { withTransaction ::
        forall a.
        (context -> m (Either DomainError a)) -> m (TransactionOutcome a)
    }

-- Infrastructure only. All injected functions use the supplied context,
-- never a separately captured connection. Map native failures to DomainError.
transactionAction ::
    (context -> m (Either DomainError a)) -> Transaction context m a
transactionAction = Transaction

newTransactionManager ::
    (Monad m) => TransactionDriver context m -> TransactionManager context m
newTransactionManager driver = TransactionManager $ \(Transaction action) -> do
    outcome <- withTransaction driver action
    pure $ case outcome of
        Committed value -> Right value
        RolledBack err -> Left err
        OutcomeUnknown reason -> Left (createTransactionOutcomeUnknown "Transaction" reason)
