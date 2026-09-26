module InvalidTransactionNesting where

import Shared.Domain.Common.Transaction
import Shared.Domain.Error (DomainError)

program :: TransactionManager context IO -> Transaction context IO (Either DomainError ())
program manager = runTransaction manager (pure ())
