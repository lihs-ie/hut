module Infrastructure.Article.DurableObject.LogOutboxSpec (run) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError (..), SQLResult (..), SQLStatement (..), SQLValue (..))
import Control.Exception (throwIO)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import Infrastructure.Article.DurableObject.LogOutbox (dispatchPendingLogsWith)
import Infrastructure.Article.DurableObject.Outbox (OutboxDispatchResult (..))
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL)
import TestSupport (check)

result :: [[SQLValue]] -> SQLResult
result rows = SQLResult [] rows 0 (length rows)

script :: [SQLResult] -> IO ExecuteSQL
script responses = fst <$> scriptWithStatements responses

scriptWithStatements :: [SQLResult] -> IO (ExecuteSQL, IO [SQLStatement])
scriptWithStatements responses = do
    remaining <- newIORef responses
    statements <- newIORef []
    let execute statement = do
            modifyIORef' statements (<> [statement])
            next <- atomicModifyIORef' remaining $ \values -> case values of
                [] -> ([], Nothing)
                value : rest -> (rest, Just value)
            maybe (fail "unexpected SQL") pure next
    pure (execute, readIORef statements)

run :: IO ()
run = do
    empty
    writesAndMarks
    rejectsBadPayload
    retriesLogFailure
    rejectsStorageFailures
    leavesAnotherBatchPending
    writesInInsertionOrder

empty :: IO ()
empty = do
    (execute, statements) <- scriptWithStatements [result [], result []]
    outcome <- dispatchPendingLogsWith execute (const (fail "no log expected"))
    check "empty log outbox" (outcome == Right (OutboxDispatchResult 0 False Nothing))
    issued <- statements
    check "log event kinds and batch limit" (case issued of
        selection : _ -> selection.parameters ==
            [SQLText "ArticleReadyToPublish", SQLText "ArticlePublished",
                SQLText "ArticleTakenDown", SQLNumber 100]
        _ -> False)

writesAndMarks :: IO ()
writesAndMarks = do
    (execute, statements) <- scriptWithStatements
        [result [[SQLText "event-1", SQLText "{\"event\":\"ArticlePublished\"}"]],
            result [[SQLText "event-1"]], result []]
    logged <- newIORef []
    outcome <- dispatchPendingLogsWith execute
        (\entry -> modifyIORef' logged (<> [entry]))
    check "log outbox marks after write" (outcome == Right (OutboxDispatchResult 1 False Nothing))
    entries <- readIORef logged
    check "log contains event" (entries == ["{\"event\":\"ArticlePublished\"}"])
    issued <- statements
    check "log mark and pending query are scoped" (case issued of
        [_selection, marked, pending] ->
            marked.parameters == [SQLText "event-1"]
                && "status = 'pending'" `Text.isInfixOf` marked.sql
                && pending.parameters ==
                    [SQLText "ArticleReadyToPublish", SQLText "ArticlePublished",
                        SQLText "ArticleTakenDown"]
        _ -> False)

rejectsBadPayload :: IO ()
rejectsBadPayload = do
    execute <- script [result [[SQLText "event-1", SQLText "invalid-json"]]]
    outcome <- dispatchPendingLogsWith execute (const (fail "invalid JSON must not be logged"))
    check "corrupt log is pending" (case outcome of
        Right (OutboxDispatchResult 0 True (Just err)) -> not (null (show err))
        _ -> False)

retriesLogFailure :: IO ()
retriesLogFailure = do
    execute <- script [result [[SQLText "event-1", SQLText "{}"]]]
    outcome <- dispatchPendingLogsWith execute
        (const (throwIO (userError "log failure")))
    check "log error retries" (case outcome of
        Right (OutboxDispatchResult 0 True (Just err)) -> not (null (show err))
        _ -> False)

rejectsStorageFailures :: IO ()
rejectsStorageFailures = do
    selected <- dispatchPendingLogsWith
        (\_ -> throwIO (SQLError "storage unavailable"))
        (const (fail "must not write"))
    check "log selection error" (case selected of
        Left err -> not (null (show err))
        _ -> False)
    invalidRow <- script [result [[SQLNumber 1, SQLText "{}"]]]
    invalid <- dispatchPendingLogsWith invalidRow (const (fail "must not write"))
    check "invalid log row" (case invalid of
        Left err -> not (null (show err))
        _ -> False)
    emptyIdentifier <- script [result [[SQLText "", SQLText "{}"]]]
    emptyKey <- dispatchPendingLogsWith emptyIdentifier (const (fail "must not write"))
    check "empty log identifier" (case emptyKey of
        Left err -> not (null (show err))
        _ -> False)
    badMark <- script [result [[SQLText "event-1", SQLText "{}"]], result []]
    marked <- dispatchPendingLogsWith badMark (const (pure ()))
    check "missing log mark retries" (case marked of
        Right (OutboxDispatchResult 0 True (Just err)) -> not (null (show err))
        _ -> False)
    badPending <- script [result [], result [[SQLNumber 1]]]
    pending <- dispatchPendingLogsWith badPending (const (fail "must not write"))
    check "invalid log pending check" (case pending of
        Left err -> not (null (show err))
        _ -> False)

leavesAnotherBatchPending :: IO ()
leavesAnotherBatchPending = do
    execute <- script
        [result [[SQLText "event-1", SQLText "{}"]],
            result [[SQLText "event-1"]], result [[SQLText "event-2"]]]
    outcome <- dispatchPendingLogsWith execute (const (pure ()))
    check "remaining logs reschedule alarm"
        (outcome == Right (OutboxDispatchResult 1 True Nothing))
    mismatch <- script
        [result [[SQLText "event-1", SQLText "{}"]],
            result [[SQLText "other"]]]
    rejected <- dispatchPendingLogsWith mismatch (const (pure ()))
    check "mismatched delivered mark retries" (case rejected of
        Right (OutboxDispatchResult 0 True (Just err)) ->
            not (null (show err))
        _ -> False)

writesInInsertionOrder :: IO ()
writesInInsertionOrder = do
    execute <- script
        [result [[SQLText "event-1", SQLText "{\"event\":1}"],
            [SQLText "event-2", SQLText "{\"event\":2}"]],
            result [[SQLText "event-1"]], result [[SQLText "event-2"]], result []]
    entries <- newIORef []
    outcome <- dispatchPendingLogsWith execute
        (\entry -> modifyIORef' entries (<> [entry]))
    check "two logs delivered" (outcome == Right (OutboxDispatchResult 2 False Nothing))
    check "logs preserve row order"
        . (== ["{\"event\":1}", "{\"event\":2}"]) =<< readIORef entries
