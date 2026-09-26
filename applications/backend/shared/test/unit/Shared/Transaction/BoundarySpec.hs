module Shared.Transaction.BoundarySpec (run) where

import Control.Monad (forM_)
import Shared.Domain.Common.Transaction
import Shared.Domain.Error
import Shared.Transaction.Support

run :: IO Bool
run = do
    forM_ [BeginFailure, CommitFailure] $ \mode -> do
        fixture <- newFixture mode
        result <- runTransaction fixture.manager program
        check "boundary failure not success" (result == Left databaseError)
        check "no durable changes" . (== Store (Just 0) []) =<< fixture.stored
        actions <- fixture.trace
        check "boundary only attempted once" (length (filter (== "begin") actions) == 1)
        check "begin failure skips callback" (mode /= BeginFailure || actions == ["begin"])

    forM_ [UnknownBefore, UnknownAfter] $ \mode -> do
        fixture <- newFixture mode
        result <- runTransaction fixture.manager program
        check
            "unknown is distinguishable"
            (result == Left (createTransactionOutcomeUnknown "Transaction" "commit acknowledgement lost"))
        actions <- fixture.trace
        check
            "unknown not retried or claimed rolled back"
            (actions == ["begin", "read", "persist", "outbox", "unknown"])
        let expected = if mode == UnknownAfter then Store (Just 1) [1] else Store (Just 0) []
        check "same unknown error may have different durable outcomes" . (== expected) =<< fixture.stored
    pure True
