module InvalidTransactionIO where

import Shared.Domain.Common.Transaction

program :: Transaction context IO ()
program = putStrLn "must not run inside a transaction"
