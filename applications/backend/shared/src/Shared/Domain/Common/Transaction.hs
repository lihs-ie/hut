module Shared.Domain.Common.Transaction (
    Transaction,
    TransactionManager,
    abort,
    fromEither,
    runTransaction,
) where

import Shared.Domain.Common.Transaction.Internal
import Shared.Domain.Error (DomainError)

abort :: (Monad m) => DomainError -> Transaction context m a
abort = fromEither . Left

fromEither :: (Monad m) => Either DomainError a -> Transaction context m a
fromEither result = Transaction (\_ -> pure result)

-- No MonadIO, recovery combinator or nested-run instruction is exposed.
runTransaction ::
    TransactionManager context m ->
    Transaction context m a ->
    m (Either DomainError a)
runTransaction (TransactionManager run) = run
