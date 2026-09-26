{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.Outbox (
    OutboxDispatchResult (..),
    SendGenerationRequest,
    dispatchPending,
    dispatchPendingWith,
) where

import Cloudflare.Workers.Binding.DurableObject (DurableObjectStorage)
import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError,
    SQLLimits (..),
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
    sqlExec,
 )
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (eitherDecodeStrict')
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import "article" Domain.Article.Common (articleIdentifierText)
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerationRequested (..),
    ExcerptGenerationRequestedMessage (..),
 )
import "shared" Shared.Domain.Error (
    DomainError,
    createServiceUnavailable,
    createUnexpectedError,
 )
import "shared" Shared.Infrastructure.Versioning (versionInteger)
import "shared" Shared.UseCase.Event (EventEnvelope (..), eventIdentifierText)

type SendGenerationRequest =
    ExcerptGenerationRequestedMessage -> IO (Either DomainError ())

data OutboxDispatchResult = OutboxDispatchResult
    { deliveredCount :: Int
    , hasPending :: Bool
    , deliveryFailure :: Maybe DomainError
    }
    deriving stock (Show, Eq)

data PendingRow = PendingRow
    { identifier :: Text
    , article :: Text
    , payload :: Text
    , revision :: SQLValue
    }

batchSize :: Int
batchSize = 100

sqlLimits :: SQLLimits
sqlLimits =
    SQLLimits
        { maximumRows = batchSize
        , maximumBytes = 16777216
        , maximumStatements = 1
        }

dispatchPending ::
    DurableObjectStorage ->
    SendGenerationRequest ->
    IO (Either DomainError OutboxDispatchResult)
dispatchPending storage = dispatchPendingWith (sqlExec storage sqlLimits)

-- Intended for the DO Alarm. Repeated sends are possible if Queue accepts a
-- message but the delivered update fails; consumers must be idempotent.
dispatchPendingWith ::
    ExecuteSQL ->
    SendGenerationRequest ->
    IO (Either DomainError OutboxDispatchResult)
dispatchPendingWith execute send = do
    selected <-
        executeSQL
            execute
            ( "SELECT identifier, article_identifier, event_kind, payload, expected_revision "
                <> "FROM article_outbox "
                <> "WHERE status = 'pending' AND event_kind = ? "
                <> "ORDER BY identifier LIMIT ?"
            )
            [SQLText generationKind, SQLNumber (fromIntegral batchSize)]
    case selected >>= traverse readPendingRow . (.rows) of
        Left err -> pure (Left err)
        Right rows -> deliverRows execute send 0 rows

deliverRows ::
    ExecuteSQL ->
    SendGenerationRequest ->
    Int ->
    [PendingRow] ->
    IO (Either DomainError OutboxDispatchResult)
deliverRows execute _ count [] = do
    remaining <- hasPendingRows execute
    pure $ (\pending -> OutboxDispatchResult count pending Nothing) <$> remaining
deliverRows execute send count (row : rest) =
    case decodeRow row of
        Left err -> pure (Right (OutboxDispatchResult count True (Just err)))
        Right message -> do
            sent <- sendSafely send message
            case sent of
                Left err -> do
                    attempted <- incrementAttempts execute row.identifier
                    pure $ Right (OutboxDispatchResult count True (Just (either id (const err) attempted)))
                Right () -> do
                    marked <- markDelivered execute row.identifier
                    case marked of
                        Left err -> pure (Right (OutboxDispatchResult count True (Just err)))
                        Right () -> deliverRows execute send (count + 1) rest

generationKind :: Text
generationKind = "ExcerptGenerationRequested"

readPendingRow :: [SQLValue] -> Either DomainError PendingRow
readPendingRow
    [SQLText identifier, SQLText article, SQLText kind, SQLText payload, revision]
        | kind == generationKind && not (Text.null identifier) =
            Right (PendingRow identifier article payload revision)
readPendingRow _ = Left (corruptOutbox "pending row has an invalid shape")

decodeRow :: PendingRow -> Either DomainError ExcerptGenerationRequestedMessage
decodeRow row = do
    message <-
        either
            (const (Left (corruptOutbox "generation request payload is invalid")))
            Right
            (eitherDecodeStrict' (encodeUtf8 row.payload))
    let ExcerptGenerationRequestedMessage
            (EventEnvelope eventIdentifier _ _ _ _ request) = message
        expectedRevision =
            SQLNumber (fromInteger (versionInteger request.expectedRevision))
    if eventIdentifierText eventIdentifier == row.identifier
        && articleIdentifierText request.article == row.article
        && row.revision == expectedRevision
        then Right message
        else Left (corruptOutbox "generation request differs from outbox metadata")

sendSafely ::
    SendGenerationRequest ->
    ExcerptGenerationRequestedMessage ->
    IO (Either DomainError ())
sendSafely send message = do
    outcome <- try @SomeException (send message)
    case outcome of
        Right result -> pure result
        Left exception
            | Just asynchronous <- fromException @SomeAsyncException exception ->
                throwIO asynchronous
            | otherwise ->
                pure (Left (createServiceUnavailable "ExcerptGenerationQueue" "send failed"))

markDelivered :: ExecuteSQL -> Text -> IO (Either DomainError ())
markDelivered execute identifier = do
    updated <-
        executeSQL
            execute
            ( "UPDATE article_outbox "
                <> "SET status = 'delivered', attempts = attempts + 1 "
                <> "WHERE identifier = ? AND status = 'pending' "
                <> "RETURNING identifier"
            )
            [SQLText identifier]
    pure (updated >>= expectUpdated identifier)

incrementAttempts :: ExecuteSQL -> Text -> IO (Either DomainError ())
incrementAttempts execute identifier = do
    updated <-
        executeSQL
            execute
            ( "UPDATE article_outbox SET attempts = attempts + 1 "
                <> "WHERE identifier = ? AND status = 'pending' "
                <> "RETURNING identifier"
            )
            [SQLText identifier]
    pure (updated >>= expectUpdated identifier)

expectUpdated :: Text -> SQLResult -> Either DomainError ()
expectUpdated identifier result =
    case result.rows of
        [[SQLText actual]] | actual == identifier -> Right ()
        _ -> Left (corruptOutbox "outbox update did not affect exactly one pending row")

hasPendingRows :: ExecuteSQL -> IO (Either DomainError Bool)
hasPendingRows execute = do
    selected <-
        executeSQL
            execute
            ( "SELECT identifier FROM article_outbox "
                <> "WHERE status = 'pending' AND event_kind = ? LIMIT 1"
            )
            [SQLText generationKind]
    pure $ selected >>= \result -> case result.rows of
        [] -> Right False
        [[SQLText _]] -> Right True
        _ -> Left (corruptOutbox "pending check returned an invalid row")

executeSQL :: ExecuteSQL -> Text -> [SQLValue] -> IO (Either DomainError SQLResult)
executeSQL execute statement parameters = do
    outcome <- try (execute SQLStatement{sql = statement, parameters})
    pure $ case outcome of
        Left (failure :: SQLError) ->
            Left (createServiceUnavailable "ArticleOutbox" (Text.pack (show failure)))
        Right result -> Right result

corruptOutbox :: Text -> DomainError
corruptOutbox = createUnexpectedError "ArticleOutbox"
