{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveAnyClass #-}

module Infrastructure.Article.DurableObject.MediaOutbox (
    MediaProjectionMessage (..),
    SendProjection,
    dispatchPendingMedia,
    dispatchPendingMediaWith,
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
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict')
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Infrastructure.Article.DurableObject.Outbox (OutboxDispatchResult (..))
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL)
import "article" Domain.Article.Common (newArticleIdentifier, newImageReference)
import "shared" Shared.Domain.Error (
    DomainError,
    createServiceUnavailable,
    createUnexpectedError,
 )

data MediaProjectionMessage = MediaProjectionMessage
    { eventIdentifier :: Text
    , sourcePosition :: Text
    , sourceKind :: Text
    , sourceIdentifier :: Text
    , referencedImages :: [Text]
    , occurredAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data StoredEvent = StoredEvent
    { identifier :: Text
    , occurredAt :: UTCTime
    , event :: StoredReferencePayload
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

data StoredReferencePayload = StoredReferencePayload
    { article :: Text
    , images :: Maybe [Text]
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

data PendingRow = PendingRow
    { position :: Text
    , identifier :: Text
    , article :: Text
    , kind :: Text
    , payload :: Text
    }

type SendProjection = MediaProjectionMessage -> IO (Either DomainError ())

batchSize :: Int
batchSize = 100

sqlLimits :: SQLLimits
sqlLimits = SQLLimits{maximumRows = batchSize, maximumBytes = 16777216, maximumStatements = 1}

dispatchPendingMedia ::
    DurableObjectStorage -> SendProjection -> IO (Either DomainError OutboxDispatchResult)
dispatchPendingMedia storage = dispatchPendingMediaWith (sqlExec storage sqlLimits)

dispatchPendingMediaWith ::
    ExecuteSQL -> SendProjection -> IO (Either DomainError OutboxDispatchResult)
dispatchPendingMediaWith execute send = do
    selected <- executeSQL execute
        ( "SELECT rowid, identifier, article_identifier, event_kind, payload "
            <> "FROM article_outbox WHERE status = 'pending' "
            <> "AND event_kind IN (?, ?, ?) ORDER BY rowid LIMIT ?"
        )
        [ SQLText "ArticleDraftStarted"
        , SQLText "ArticleDraftAmended"
        , SQLText "ArticleDiscarded"
        , SQLNumber (fromIntegral batchSize)
        ]
    case selected >>= traverse readPendingRow . (.rows) of
        Left err -> pure (Left err)
        Right rows -> deliverRows execute send 0 rows

deliverRows ::
    ExecuteSQL -> SendProjection -> Int -> [PendingRow] ->
    IO (Either DomainError OutboxDispatchResult)
deliverRows execute _ count [] = do
    remaining <- hasPendingRows execute
    pure $ (\pending -> OutboxDispatchResult count pending Nothing) <$> remaining
deliverRows execute send count (row : rest) =
    case decodeRow row of
        Left err -> pure (Right (OutboxDispatchResult count True (Just err)))
        Right projection -> do
            sent <- sendSafely send projection
            case sent of
                Left err -> do
                    attempted <- incrementAttempts execute row.identifier
                    pure $ Right (OutboxDispatchResult count True (Just (either id (const err) attempted)))
                Right () -> do
                    marked <- markDelivered execute row.identifier
                    case marked of
                        Left err -> pure (Right (OutboxDispatchResult count True (Just err)))
                        Right () -> deliverRows execute send (count + 1) rest

readPendingRow :: [SQLValue] -> Either DomainError PendingRow
readPendingRow [SQLNumber rawPosition, SQLText identifier, SQLText article, SQLText kind, SQLText payload]
    | not (isNaN rawPosition || isInfinite rawPosition)
        && rawPosition >= 1
        && rawPosition <= 9007199254740991
        && fromInteger (round rawPosition) == rawPosition
        && not (Text.null identifier) =
        Right (PendingRow (Text.pack (show (round rawPosition :: Integer))) identifier article kind payload)
readPendingRow _ = Left (corruptOutbox "projection row has an invalid shape")

decodeRow :: PendingRow -> Either DomainError MediaProjectionMessage
decodeRow row = do
    stored <- either (const (Left (corruptOutbox "projection payload is invalid"))) Right
        (eitherDecodeStrict' (encodeUtf8 row.payload) :: Either String StoredEvent)
    _ <- either (const (Left (corruptOutbox "article identifier is invalid"))) Right
        (newArticleIdentifier row.article)
    if stored.identifier /= row.identifier || stored.event.article /= row.article
        then Left (corruptOutbox "projection differs from outbox metadata")
        else do
            images <- case row.kind of
                "ArticleDraftStarted" -> requiredImages stored.event.images
                "ArticleDraftAmended" -> requiredImages stored.event.images
                "ArticleDiscarded" -> case stored.event.images of
                    Nothing -> Right []
                    Just [] -> Right []
                    Just _ -> Left (corruptOutbox "discard projection must clear images")
                _ -> Left (corruptOutbox "unknown projection event")
            _ <- traverse (either (const (Left (corruptOutbox "image identifier is invalid"))) Right
                . newImageReference) images
            pure MediaProjectionMessage
                { eventIdentifier = row.identifier
                , sourcePosition = row.position
                , sourceKind = "article"
                , sourceIdentifier = row.article
                , referencedImages = images
                , occurredAt = stored.occurredAt
                }
  where
    requiredImages = maybe (Left (corruptOutbox "draft projection is missing images")) Right

sendSafely :: SendProjection -> MediaProjectionMessage -> IO (Either DomainError ())
sendSafely send message = do
    result <- try @SomeException (send message)
    case result of
        Right outcome -> pure outcome
        Left exception
            | Just asynchronous <- fromException @SomeAsyncException exception ->
                throwIO asynchronous
            | otherwise ->
                pure (Left (createServiceUnavailable "MediaProjectionQueue" "send failed"))

markDelivered :: ExecuteSQL -> Text -> IO (Either DomainError ())
markDelivered execute identifier = do
    updated <- executeSQL execute
        ( "UPDATE article_outbox SET status = 'delivered', attempts = attempts + 1 "
            <> "WHERE identifier = ? AND status = 'pending' RETURNING identifier"
        )
        [SQLText identifier]
    pure (updated >>= expectUpdated identifier)

incrementAttempts :: ExecuteSQL -> Text -> IO (Either DomainError ())
incrementAttempts execute identifier = do
    updated <- executeSQL execute
        ( "UPDATE article_outbox SET attempts = attempts + 1 "
            <> "WHERE identifier = ? AND status = 'pending' RETURNING identifier"
        )
        [SQLText identifier]
    pure (updated >>= expectUpdated identifier)

expectUpdated :: Text -> SQLResult -> Either DomainError ()
expectUpdated identifier result = case result.rows of
    [[SQLText actual]] | actual == identifier -> Right ()
    _ -> Left (corruptOutbox "projection update did not affect one pending row")

hasPendingRows :: ExecuteSQL -> IO (Either DomainError Bool)
hasPendingRows execute = do
    selected <- executeSQL execute
        ( "SELECT identifier FROM article_outbox WHERE status = 'pending' "
            <> "AND event_kind IN (?, ?, ?) LIMIT 1"
        )
        [SQLText "ArticleDraftStarted", SQLText "ArticleDraftAmended", SQLText "ArticleDiscarded"]
    pure $ selected >>= \result -> case result.rows of
        [] -> Right False
        [[SQLText _]] -> Right True
        _ -> Left (corruptOutbox "projection pending check returned an invalid row")

executeSQL :: ExecuteSQL -> Text -> [SQLValue] -> IO (Either DomainError SQLResult)
executeSQL execute statement parameters = do
    outcome <- try (execute SQLStatement{sql = statement, parameters})
    pure $ case outcome of
        Left (failure :: SQLError) ->
            Left (createServiceUnavailable "MediaProjectionOutbox" (Text.pack (show failure)))
        Right result -> Right result

corruptOutbox :: Text -> DomainError
corruptOutbox = createUnexpectedError "MediaProjectionOutbox"
