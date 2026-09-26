module Shared.Transaction.FailureSpec (run) where

import Control.Monad (forM_)
import Shared.Domain.Common.Transaction
import Shared.Transaction.Support

run :: IO Bool
run = do
    forM_ [failDatabase, nativeFailure] $ \operation -> do
        fixture <- newFixture Normal
        result <- runTransaction fixture.manager $ do
            persistArticle 9
            operation
            appendOutbox 9
            pure (42 :: Int)
        check "database failure cannot be discarded by following pure" (result == Left databaseError)
        check "failed transaction leaves original store" . (== Store (Just 0) []) =<< fixture.stored
        actions <- fixture.trace
        check
            "no outbox or commit after failure"
            (not ("outbox" `elem` actions) && last actions == "rollback")

    fixture <- newFixture Normal
    result <- runTransaction fixture.manager $ do
        persistArticle 7
        fromEither (Left businessError)
        appendOutbox 7
    check "business error preserved" (result == Left businessError)
    check "business abort rolls back" . (== Store (Just 0) []) =<< fixture.stored
    check "no continuation after domain failure"
        . (== ["begin", "persist", "rollback"])
        =<< fixture.trace

    immediate <- newFixture Normal
    stopped <- runTransaction immediate.manager (abort businessError >> findArticle)
    check "abort short circuits bind" (stopped == Left businessError)
    check "abort does not query" . (== ["begin", "rollback"]) =<< immediate.trace

    applicative <- newFixture Normal
    failed <-
        runTransaction
            applicative.manager
            ((abort businessError :: Transaction TestContext IO (() -> ())) <*> persistArticle 9)
    check "Applicative short circuits" (failed == Left businessError)
    check "no write after failed function" . (== ["begin", "rollback"]) =<< applicative.trace

    later <- newFixture Normal
    stoppedLater <-
        runTransaction
            later.manager
            ((\_ value -> value) <$> persistArticle 4 <*> abort businessError :: Transaction TestContext IO Int)
    check "failure of second Applicative operand" (stoppedLater == Left businessError)
    check "earlier write rolled back" . (== Store (Just 0) []) =<< later.stored
    pure True
