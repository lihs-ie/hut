module ValidTransaction where

import Shared.Domain.Common.Transaction
import Shared.Domain.Error (DomainError)

program :: (Monad m) => Transaction context m Int
program = (+ 1) <$> pure 1
run :: (Monad m) => TransactionManager context m -> m (Either DomainError Int)
run manager = runTransaction manager program
