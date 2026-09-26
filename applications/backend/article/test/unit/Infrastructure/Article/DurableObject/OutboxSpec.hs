{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.OutboxSpec (run) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError (..),
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
 )
import Control.Exception (AsyncException (ThreadKilled), SomeException, throwIO, try)
import Control.Monad (forM_, unless)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import "article" Domain.Article.Common (newArticleIdentifier)
import Infrastructure.Article.DurableObject.Outbox
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerationRequested (..),
    ExcerptGenerationRequestedMessage (..),
    newGenerationRequestIdentifier,
 )
import "shared" Shared.Domain.Common.Primitive (newPositiveInteger)
import "shared" Shared.Domain.Error (DomainError (..), createServiceUnavailable)
import "shared" Shared.Infrastructure.Versioning (newVersion)
import "shared" Shared.UseCase.Context (newActor, newCorrelationIdentifier)
import "shared" Shared.UseCase.Event (newEventEnvelope, newEventIdentifier)

check :: String -> Bool -> IO ()
check label condition = unless condition (fail label)

right :: (Show errorType) => Either errorType value -> IO value
right = either (fail . show) pure

timestamp :: UTCTime
timestamp = read "2026-01-01 00:00:00 UTC"

requestMessage :: Text -> IO ExcerptGenerationRequestedMessage
requestMessage eventText = do
    event <- right (newEventIdentifier eventText)
    request <- right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    article <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    version <- newVersion <$> right (newPositiveInteger 3)
    actor <- right (newActor "admin")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    pure $ ExcerptGenerationRequestedMessage $
        newEventEnvelope
            event
            timestamp
            actor
            correlation
            Nothing
            (ExcerptGenerationRequested request article version)

payload :: ExcerptGenerationRequestedMessage -> Text
payload = decodeUtf8 . Lazy.toStrict . encode

pendingRow :: Text -> ExcerptGenerationRequestedMessage -> [SQLValue]
pendingRow identifier message =
    [ SQLText identifier
    , SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"
    , SQLText "ExcerptGenerationRequested"
    , SQLText (payload message)
    , SQLNumber 3
    ]

emptyResult :: SQLResult
emptyResult = SQLResult [] [] 0 0

resultRows :: [[SQLValue]] -> SQLResult
resultRows rows = SQLResult [] rows 0 (length rows)

newScript :: [SQLResult] -> IO (ExecuteSQL, IO [SQLStatement])
newScript responses = do
    remaining <- newIORef responses
    statements <- newIORef []
    let execute statement = do
            modifyIORef' statements (<> [statement])
            next <- atomicModifyIORef' remaining $ \values -> case values of
                [] -> ([], Nothing)
                value : rest -> (rest, Just value)
            maybe (fail "unexpected SQL statement") pure next
    pure (execute, readIORef statements)

firstStatement :: [SQLStatement] -> IO SQLStatement
firstStatement (statement : _) = pure statement
firstStatement [] = fail "expected a SQL statement"

secondStatement :: [SQLStatement] -> IO SQLStatement
secondStatement (_ : statement : _) = pure statement
secondStatement _ = fail "expected a second SQL statement"

run :: IO ()
run = do
    emptyOutbox
    sendsInIdentifierOrder
    keepsFailedSendPending
    stopsAfterPartialDelivery
    rejectsInvalidPayload
    rejectsMismatchedMetadata
    rejectsMismatchedRevision
    rejectsWrongEventKind
    retriesFailedDeliveryMark
    reportsRemainingWork
    handlesQueueException
    rejectsMalformedSelection
    rejectsCorruptOutboxAndSQLFailures

emptyOutbox :: IO ()
emptyOutbox = do
    (execute, statements) <- newScript [emptyResult, emptyResult]
    outcome <- dispatchPendingWith execute (\_ -> fail "empty outbox must not send")
    check "empty outbox" (outcome == Right (OutboxDispatchResult 0 False Nothing))
    issued <- statements
    selection <- firstStatement issued
    check "empty selection ordered" ("ORDER BY identifier" `Text.isInfixOf` selection.sql)
    check "only generation requests selected"
        (SQLText "ExcerptGenerationRequested" `elem` selection.parameters)

sendsInIdentifierOrder :: IO ()
sendsInIdentifierOrder = do
    first <- requestMessage "event-1"
    second <- requestMessage "event-2"
    let rows = [pendingRow "event-1" first, pendingRow "event-2" second]
    (execute, statements) <-
        newScript
            [ resultRows rows
            , resultRows [[SQLText "event-1"]]
            , resultRows [[SQLText "event-2"]]
            , emptyResult
            ]
    sent <- newIORef []
    outcome <- dispatchPendingWith execute $ \message -> do
        beforeSend <- statements
        check "row remains pending until send succeeds"
            (length beforeSend == 1 || length beforeSend == 2)
        modifyIORef' sent (<> [message])
        pure (Right ())
    check "two rows delivered" (outcome == Right (OutboxDispatchResult 2 False Nothing))
    check "messages delivered in order" =<< (== [first, second]) <$> readIORef sent
    issued <- statements
    firstUpdate <- secondStatement issued
    check "send followed by conditional delivered mark"
        (length issued == 4 && "status = 'delivered'" `Text.isInfixOf` firstUpdate.sql)
    check "first update identifies first row"
        (firstUpdate.parameters == [SQLText "event-1"])

keepsFailedSendPending :: IO ()
keepsFailedSendPending = do
    first <- requestMessage "event-1"
    second <- requestMessage "event-2"
    let failure = createServiceUnavailable "Queue" "temporarily unavailable"
    (execute, statements) <-
        newScript
            [ resultRows [pendingRow "event-1" first, pendingRow "event-2" second]
            , resultRows [[SQLText "event-1"]]
            ]
    sent <- newIORef []
    outcome <- dispatchPendingWith execute $ \message -> do
        modifyIORef' sent (<> [message])
        pure (Left failure)
    check "failed send requests retry"
        (outcome == Right (OutboxDispatchResult 0 True (Just failure)))
    check "later row is not sent after failure" =<< (== [first]) <$> readIORef sent
    issued <- statements
    attemptUpdate <- secondStatement issued
    check "failure only increments attempts"
        ( length issued == 2
            && "attempts = attempts + 1" `Text.isInfixOf` attemptUpdate.sql
            && not ("status = 'delivered'" `Text.isInfixOf` attemptUpdate.sql)
        )

stopsAfterPartialDelivery :: IO ()
stopsAfterPartialDelivery = do
    first <- requestMessage "event-1"
    second <- requestMessage "event-2"
    let failure = createServiceUnavailable "Queue" "temporarily unavailable"
    (execute, statements) <-
        newScript
            [ resultRows [pendingRow "event-1" first, pendingRow "event-2" second]
            , resultRows [[SQLText "event-1"]]
            , resultRows [[SQLText "event-2"]]
            ]
    attempts <- newIORef (0 :: Int)
    outcome <- dispatchPendingWith execute $ \_ -> do
        attempt <- atomicModifyIORef' attempts $ \count -> (count + 1, count)
        pure (if attempt == 0 then Right () else Left failure)
    check "partial delivery retains the failed row"
        (outcome == Right (OutboxDispatchResult 1 True (Just failure)))
    issued <- statements
    check "partial delivery updates each attempted row only" (length issued == 3)

rejectsInvalidPayload :: IO ()
rejectsInvalidPayload = do
    message <- requestMessage "event-1"
    let row = replacePayload "{}" (pendingRow "event-1" message)
    (execute, statements) <- newScript [resultRows [row]]
    outcome <- dispatchPendingWith execute (\_ -> fail "invalid JSON must not send")
    check "invalid payload stays pending" (isFailure outcome)
    check "invalid payload makes no write" . (== 1) . length =<< statements

rejectsMismatchedMetadata :: IO ()
rejectsMismatchedMetadata = do
    message <- requestMessage "event-1"
    let row = pendingRow "other-event" message
    (execute, statements) <- newScript [resultRows [row]]
    outcome <- dispatchPendingWith execute (\_ -> fail "mismatched metadata must not send")
    check "metadata mismatch stays pending" (isFailure outcome)
    check "metadata mismatch makes no write" . (== 1) . length =<< statements

rejectsMismatchedRevision :: IO ()
rejectsMismatchedRevision = do
    message <- requestMessage "event-1"
    let row = replaceRevision (SQLNumber 4) (pendingRow "event-1" message)
    (execute, statements) <- newScript [resultRows [row]]
    outcome <- dispatchPendingWith execute (\_ -> fail "wrong revision must not send")
    check "revision mismatch stays pending" (isFailure outcome)
    check "revision mismatch makes no write" . (== 1) . length =<< statements

rejectsWrongEventKind :: IO ()
rejectsWrongEventKind = do
    message <- requestMessage "event-1"
    let row = replaceKind "ArticleProofreaded" (pendingRow "event-1" message)
    (execute, _) <- newScript [resultRows [row]]
    outcome <- dispatchPendingWith execute (\_ -> fail "wrong event kind must not send")
    check "wrong event kind rejected" (isLeft outcome)

retriesFailedDeliveryMark :: IO ()
retriesFailedDeliveryMark = do
    message <- requestMessage "event-1"
    (execute, statements) <- newScript [resultRows [pendingRow "event-1" message], emptyResult]
    sent <- newIORef (0 :: Int)
    outcome <- dispatchPendingWith execute $ \_ -> do
        modifyIORef' sent (+ 1)
        pure (Right ())
    check "queue was sent once" =<< (== 1) <$> readIORef sent
    check "failed mark requests retry" (isFailure outcome)
    check "failed mark is attempted" . (== 2) . length =<< statements

reportsRemainingWork :: IO ()
reportsRemainingWork = do
    message <- requestMessage "event-1"
    (execute, _) <-
        newScript
            [ resultRows [pendingRow "event-1" message]
            , resultRows [[SQLText "event-1"]]
            , resultRows [[SQLText "event-2"]]
            ]
    outcome <- dispatchPendingWith execute (\_ -> pure (Right ()))
    check "more pending work reschedules Alarm"
        (outcome == Right (OutboxDispatchResult 1 True Nothing))

handlesQueueException :: IO ()
handlesQueueException = do
    message <- requestMessage "event-1"
    (execute, statements) <-
        newScript
            [ resultRows [pendingRow "event-1" message]
            , resultRows [[SQLText "event-1"]]
            ]
    outcome <- dispatchPendingWith execute (\_ -> ioError (userError "Queue disconnected"))
    check "queue exception requests retry" (isFailure outcome)
    check "queue exception does not mark delivered"
        . all (not . Text.isInfixOf "status = 'delivered'" . (.sql))
        =<< statements
    (asyncSQL, _) <- newScript [resultRows [pendingRow "event-1" message]]
    asyncFailure <- try (dispatchPendingWith asyncSQL (\_ -> throwIO ThreadKilled))
        :: IO (Either SomeException (Either DomainError OutboxDispatchResult))
    check "asynchronous queue exception propagates" (either (const True) (const False) asyncFailure)

rejectsMalformedSelection :: IO ()
rejectsMalformedSelection = do
    (execute, _) <- newScript [resultRows [[SQLText "event-1"]]]
    outcome <- dispatchPendingWith execute (\_ -> fail "malformed row must not send")
    check "malformed row is an SQL data error" (isLeft outcome)

rejectsCorruptOutboxAndSQLFailures :: IO ()
rejectsCorruptOutboxAndSQLFailures = do
    message <- requestMessage "event-1"
    forM_
        [ ("empty event identifier", replaceIdentifier "")
        , ("wrong article identifier", replaceArticle "01ARZ3NDEKTSV4RRFFQ69G5FAX")
        ] $ \(label, change) -> do
            (execute, issued) <- newScript [resultRows [change (pendingRow "event-1" message)]]
            outcome <- dispatchPendingWith execute (\_ -> fail "corrupt outbox must not send")
            check label (isLeft outcome || isFailure outcome)
            check "corrupt outbox skips write" =<< (== 1) . length <$> issued

    (remainingSQL, _) <- newScript [emptyResult, resultRows [[SQLNumber 3]]]
    remaining <- dispatchPendingWith remainingSQL (\_ -> fail "empty outbox must not send")
    check "malformed pending check rejected" (isLeft remaining)

    (attemptSQL, issued) <- newScript
        [resultRows [pendingRow "event-1" message], emptyResult]
    failedAttempt <- dispatchPendingWith attemptSQL
        (\_ -> pure (Left (createServiceUnavailable "Queue" "unavailable")))
    check "failed attempt counter triggers retry" (isFailure failedAttempt)
    check "attempt counter write issued" =<< (== 2) . length <$> issued

    selectionFailure <- dispatchPendingWith
        (\_ -> throwIO (SQLError "storage unavailable"))
        (\_ -> fail "selection failure must not send")
    check "SQL exception maps to service error" $ case selectionFailure of
        Left ServiceUnavailable{} -> True
        _ -> False

replaceIdentifier :: Text -> [SQLValue] -> [SQLValue]
replaceIdentifier value (_ : rest) = SQLText value : rest
replaceIdentifier _ row = row

replaceArticle :: Text -> [SQLValue] -> [SQLValue]
replaceArticle value (identifier : _ : rest) = identifier : SQLText value : rest
replaceArticle _ row = row

replacePayload :: Text -> [SQLValue] -> [SQLValue]
replacePayload value [identifier, article, kind, _, revision] =
    [identifier, article, kind, SQLText value, revision]
replacePayload _ row = row

replaceKind :: Text -> [SQLValue] -> [SQLValue]
replaceKind value [identifier, article, _, payloadValue, revision] =
    [identifier, article, SQLText value, payloadValue, revision]
replaceKind _ row = row

replaceRevision :: SQLValue -> [SQLValue] -> [SQLValue]
replaceRevision value [identifier, article, kind, payloadValue, _] =
    [identifier, article, kind, payloadValue, value]
replaceRevision _ row = row

isFailure :: Either DomainError OutboxDispatchResult -> Bool
isFailure (Right (OutboxDispatchResult 0 True (Just (UnexpectedError _)))) = True
isFailure (Right (OutboxDispatchResult 0 True (Just (ServiceUnavailable _)))) = True
isFailure _ = False

isLeft :: Either errorType value -> Bool
isLeft (Left _) = True
isLeft _ = False
