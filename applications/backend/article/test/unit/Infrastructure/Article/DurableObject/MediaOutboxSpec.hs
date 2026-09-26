module Infrastructure.Article.DurableObject.MediaOutboxSpec (run) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError (..),
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
 )
import Control.Exception (throwIO)
import Data.Aeson (decode, encode, object, (.=))
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Infrastructure.Article.DurableObject.MediaOutbox
import Infrastructure.Article.DurableObject.Outbox (OutboxDispatchResult (..))
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL)
import Shared.Domain.Error (DomainError, createServiceUnavailable)
import TestSupport (check)

article :: Text
article = "01ARZ3NDEKTSV4RRFFQ69G5FAV"

image :: Text
image = "01ARZ3NDEKTSV4RRFFQ69G5FAW"

payload :: Text -> Maybe [Text] -> Text
payload identifier images = decodeUtf8 $ Lazy.toStrict $ encode $ object
    [ "identifier" .= identifier
    , "occurredAt" .= ("2026-01-01T00:00:00Z" :: Text)
    , "event" .= object ["article" .= article, "images" .= images]
    ]

row :: Text -> Text -> Maybe [Text] -> [SQLValue]
row position kind images =
    [SQLNumber (read (Text.unpack position)), SQLText "event-1", SQLText article,
        SQLText kind, SQLText (payload "event-1" images)]

result :: [[SQLValue]] -> SQLResult
result rows = SQLResult [] rows 0 (length rows)

script :: [SQLResult] -> IO (ExecuteSQL, IO [SQLStatement])
script responses = do
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
    sendsProjection
    discardClearsReferences
    retriesFailures
    rejectsCorruptRows
    rejectsStorageFailures
    leavesAnotherBatchPending
    stopsAfterPartialDelivery

empty :: IO ()
empty = do
    (execute, statements) <- script [result [], result []]
    outcome <- dispatchPendingMediaWith execute (const (fail "no message expected"))
    check "empty media outbox" (outcome == Right (OutboxDispatchResult 0 False Nothing))
    issued <- statements
    check "media selection uses insertion order" (case issued of
        first : _ -> "ORDER BY rowid" `Text.isInfixOf` first.sql
            && first.parameters ==
                [SQLText "ArticleDraftStarted", SQLText "ArticleDraftAmended",
                    SQLText "ArticleDiscarded", SQLNumber 100]
        _ -> False)

sendsProjection :: IO ()
sendsProjection = do
    (execute, statements) <- script
        [result [row "17" "ArticleDraftStarted" (Just [image])],
            result [[SQLText "event-1"]], result []]
    sent <- newIORef []
    outcome <- dispatchPendingMediaWith execute $ \message -> do
        modifyIORef' sent (<> [message])
        pure (Right ())
    check "projection delivered" (outcome == Right (OutboxDispatchResult 1 False Nothing))
    messages <- readIORef sent
    check "projection contract" (case messages of
        [message] -> message.eventIdentifier == "event-1"
            && message.sourcePosition == "17"
            && message.sourceKind == "article"
            && message.sourceIdentifier == article
            && message.referencedImages == [image]
            && message.occurredAt == read "2026-01-01 00:00:00 UTC"
            && decode (encode message) == Just message
            && "MediaProjectionMessage" `Text.isInfixOf` Text.pack (show message)
        _ -> False)
    issued <- statements
    check "delivery mark follows send" (case issued of
        [_selection, marked, pending] ->
            "status = 'delivered'" `Text.isInfixOf` marked.sql
                && marked.parameters == [SQLText "event-1"]
                && pending.parameters ==
                    [SQLText "ArticleDraftStarted", SQLText "ArticleDraftAmended",
                        SQLText "ArticleDiscarded"]
        _ -> False)

discardClearsReferences :: IO ()
discardClearsReferences = do
    (execute, _) <- script
        [result [row "18" "ArticleDiscarded" Nothing],
            result [[SQLText "event-1"]], result []]
    messages <- newIORef []
    outcome <- dispatchPendingMediaWith execute $ \message -> do
        modifyIORef' messages (<> [message])
        pure (Right ())
    check "discard delivered" (outcome == Right (OutboxDispatchResult 1 False Nothing))
    delivered <- readIORef messages
    check "discard clears references" (case delivered of
        [message] -> null message.referencedImages
        _ -> False)

retriesFailures :: IO ()
retriesFailures = do
    let unavailable = createServiceUnavailable "Media" "down"
    (execute, statements) <- script
        [result [row "1" "ArticleDraftAmended" (Just [])],
            result [[SQLText "event-1"]]]
    outcome <- dispatchPendingMediaWith execute (const (pure (Left unavailable)))
    check "failed queue send stays pending"
        (outcome == Right (OutboxDispatchResult 0 True (Just unavailable)))
    issued <- statements
    check "only attempts change" (case issued of
        [_selection, attempted] ->
            "attempts = attempts + 1" `Text.isInfixOf` attempted.sql
                && not ("status = 'delivered'" `Text.isInfixOf` attempted.sql)
        _ -> False)
    (throws, _) <- script
        [result [row "1" "ArticleDraftAmended" (Just [])],
            result [[SQLText "event-1"]]]
    raised <- dispatchPendingMediaWith throws
        (const (throwIO (userError "queue failure")))
    check "queue exception is retried" (case raised of
        Right (OutboxDispatchResult 0 True (Just err)) -> not (null (show err))
        _ -> False)

rejectsCorruptRows :: IO ()
rejectsCorruptRows = do
    let badRows =
            [ [SQLText "not-position", SQLText "event-1", SQLText article,
                SQLText "ArticleDraftStarted", SQLText (payload "event-1" (Just []))]
            , row "1" "ArticleDraftStarted" Nothing
            , row "1" "ArticleDiscarded" (Just [image])
            , row "1" "Unknown" (Just [])
            , row "1" "ArticleDraftStarted" (Just ["invalid"])
            , [SQLNumber 1, SQLText "event-1", SQLText article,
                SQLText "ArticleDraftStarted", SQLText "not-json"]
            , [SQLNumber 1, SQLText "other", SQLText article,
                SQLText "ArticleDraftStarted", SQLText (payload "event-1" (Just []))]
            , [SQLNumber 1, SQLText "event-1", SQLText "invalid-article",
                SQLText "ArticleDraftStarted", SQLText (payload "event-1" (Just []))]
            , [SQLNumber 1.5, SQLText "event-1", SQLText article,
                SQLText "ArticleDraftStarted", SQLText (payload "event-1" (Just []))]
            , [SQLNumber (-1), SQLText "event-1", SQLText article,
                SQLText "ArticleDraftStarted", SQLText (payload "event-1" (Just []))]
            , [SQLNumber (0 / 0), SQLText "event-1", SQLText article,
                SQLText "ArticleDraftStarted", SQLText (payload "event-1" (Just []))]
            , [SQLNumber (1 / 0), SQLText "event-1", SQLText article,
                SQLText "ArticleDraftStarted", SQLText (payload "event-1" (Just []))]
            , [SQLNumber 9007199254740992, SQLText "event-1", SQLText article,
                SQLText "ArticleDraftStarted", SQLText (payload "event-1" (Just []))]
            , [SQLNumber 1, SQLText "", SQLText article,
                SQLText "ArticleDraftStarted", SQLText (payload "event-1" (Just []))]
            , [SQLNumber 1, SQLText "event-1", SQLText article,
                SQLText "ArticleDraftStarted", SQLText (decodeUtf8 $ Lazy.toStrict $ encode $ object
                    ["identifier" .= ("event-1" :: Text),
                        "occurredAt" .= ("2026-01-01T00:00:00Z" :: Text),
                        "event" .= object ["article" .= ("other" :: Text),
                            "images" .= Just ([] :: [Text])]])]
            ]
    mapM_ reject badRows
  where
    reject bad = do
        (execute, _) <- script [result [bad]]
        outcome <- dispatchPendingMediaWith execute (const (fail "invalid row must not send"))
        check "invalid media row rejected" (case outcome of
            Left (err :: DomainError) -> not (null (show err))
            Right (OutboxDispatchResult 0 True (Just err)) -> not (null (show err))
            _ -> False)

rejectsStorageFailures :: IO ()
rejectsStorageFailures = do
    let unavailable _ = throwIO (SQLError "storage failed")
    selected <- dispatchPendingMediaWith unavailable (const (fail "must not send"))
    check "selection failure is a domain error" (case selected of
        Left err -> not (null (show err))
        _ -> False)
    (badMark, _) <- script
        [result [row "1" "ArticleDraftStarted" (Just [])], result []]
    marked <- dispatchPendingMediaWith badMark (const (pure (Right ())))
    check "missing conditional mark retries" (case marked of
        Right (OutboxDispatchResult 0 True (Just err)) -> not (null (show err))
        _ -> False)
    (badAttempt, _) <- script
        [result [row "1" "ArticleDraftStarted" (Just [])], result []]
    failed <- dispatchPendingMediaWith badAttempt
        (const (pure (Left (createServiceUnavailable "Media" "down"))))
    check "missing attempt update retries" (case failed of
        Right (OutboxDispatchResult 0 True (Just err)) -> not (null (show err))
        _ -> False)
    (badPending, _) <- script [result [], result [[SQLNumber 1]]]
    pending <- dispatchPendingMediaWith badPending (const (fail "must not send"))
    check "bad pending row fails" (case pending of
        Left err -> not (null (show err))
        _ -> False)

leavesAnotherBatchPending :: IO ()
leavesAnotherBatchPending = do
    (execute, _) <- script
        [result [row "21" "ArticleDraftAmended" (Just [])],
            result [[SQLText "event-1"]], result [[SQLText "event-2"]]]
    outcome <- dispatchPendingMediaWith execute (const (pure (Right ())))
    check "remaining projections reschedule alarm"
        (outcome == Right (OutboxDispatchResult 1 True Nothing))
    (mismatch, _) <- script
        [result [row "21" "ArticleDraftAmended" (Just [])],
            result [[SQLText "other"]]]
    rejected <- dispatchPendingMediaWith mismatch (const (pure (Right ())))
    check "mismatched delivered mark retries" (case rejected of
        Right (OutboxDispatchResult 0 True (Just err)) ->
            not (null (show err))
        _ -> False)
    (clear, _) <- script
        [result [row "22" "ArticleDiscarded" (Just [])],
            result [[SQLText "event-1"]], result []]
    cleared <- dispatchPendingMediaWith clear (const (pure (Right ())))
    check "explicit empty discard projection" (cleared == Right
        (OutboxDispatchResult 1 False Nothing))

stopsAfterPartialDelivery :: IO ()
stopsAfterPartialDelivery = do
    let second = [SQLNumber 32, SQLText "event-2", SQLText article,
            SQLText "ArticleDraftAmended", SQLText (payload "event-2" (Just []))]
        unavailable = createServiceUnavailable "Media" "queue unavailable"
    (execute, statements) <- script
        [result [row "31" "ArticleDraftStarted" (Just [image]), second],
            result [[SQLText "event-1"]], result [[SQLText "event-2"]]]
    delivered <- newIORef []
    outcome <- dispatchPendingMediaWith execute $ \message -> do
        modifyIORef' delivered (<> [message.eventIdentifier])
        pure (if message.eventIdentifier == "event-2"
            then Left unavailable else Right ())
    check "partial media delivery is retried" (outcome == Right
        (OutboxDispatchResult 1 True (Just unavailable)))
    check "media event order" . (== ["event-1", "event-2"]) =<< readIORef delivered
    issued <- statements
    check "first event delivered and second only attempted" (case issued of
        [_selection, marked, attempted] ->
            "status = 'delivered'" `Text.isInfixOf` marked.sql
                && not ("status = 'delivered'" `Text.isInfixOf` attempted.sql)
        _ -> False)
