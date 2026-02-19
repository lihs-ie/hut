{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module API.Request (EventRequest (..), PubSubPushRequest, decodePushRequest, toDomain) where

import Data.Aeson (FromJSON (..), Value, eitherDecodeStrict, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS
import Data.Time (UTCTime)
import Domain.Common (Timeline (Timeline, createdAt, updatedAt))
import Domain.Event (ArticleCreatedPayload (..), ArticleEditedPayload (..), Event (..), EventPayload (ArticleCreatedPayload', ArticleEditedPayload', ArticleTerminatePayload', MemoCreatedPayload', MemoEditedPayload', MemoTerminatePayload'), EventType (ArticleCreated, ArticleEdited, ArticleTerminated, MemoCreated, MemoEdited, MemoTerminated), MemoCreatedPayload (..), MemoEditedPayload (..), MemoEntry (MemoEntry, createdAt, text))
import GHC.Generics (Generic)

data TimelineRequest = TimelineRequest
  { createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

instance FromJSON TimelineRequest

data MemoEntryRequest = MemoEntryRequest
  { text :: String,
    createdAt :: UTCTime
  }
  deriving (Generic)

instance FromJSON MemoEntryRequest

data ArticlePayloadRequest = ArticlePayloadRequest
  { identifier :: String,
    title :: String,
    content :: String,
    excerpt :: String,
    slug :: String,
    status :: String,
    tags :: [String],
    timeline :: TimelineRequest
  }
  deriving (Generic)

instance FromJSON ArticlePayloadRequest

data MemoPayloadRequest = MemoPayloadRequest
  { identifier :: String,
    title :: String,
    slug :: String,
    entries :: [MemoEntryRequest],
    tags :: [String],
    status :: String,
    timeline :: TimelineRequest
  }
  deriving (Generic)

instance FromJSON MemoPayloadRequest

newtype SnapshotPayloadRequest a = SnapshotPayloadRequest
  { snapshot :: a
  }
  deriving (Generic)

instance (FromJSON a) => FromJSON (SnapshotPayloadRequest a)

data EditedPayloadRequest a = EditedPayloadRequest
  { next :: a,
    before :: a
  }
  deriving (Generic)

instance (FromJSON a) => FromJSON (EditedPayloadRequest a)

newtype ArticleTerminatedPayloadRequest = ArticleTerminatedPayloadRequest
  { article :: String
  }
  deriving (Generic)

instance FromJSON ArticleTerminatedPayloadRequest

newtype MemoTerminatedPayloadRequest = MemoTerminatedPayloadRequest
  { memo :: String
  }
  deriving (Generic)

instance FromJSON MemoTerminatedPayloadRequest

data EventRequest = EventRequest
  { identifier :: String,
    occurredAt :: UTCTime,
    eventType :: String,
    payload :: Value
  }

instance FromJSON EventRequest where
  parseJSON = withObject "EventRequest" $ \v ->
    EventRequest
      <$> v .: "identifier"
      <*> v .: "occurredAt"
      <*> v .: "type"
      <*> v .: "payload"

toTimeline :: TimelineRequest -> Timeline
toTimeline request =
  Timeline {createdAt = request.createdAt, updatedAt = request.updatedAt}

toMemoEntry :: MemoEntryRequest -> MemoEntry
toMemoEntry request =
  MemoEntry {text = request.text, createdAt = request.createdAt}

toArticleCreatedPayload :: ArticlePayloadRequest -> ArticleCreatedPayload
toArticleCreatedPayload request =
  ArticleCreatedPayload
    { identifier = request.identifier,
      title = request.title,
      content = request.content,
      excerpt = request.excerpt,
      slug = request.slug,
      status = request.status,
      tags = request.tags,
      timeline = toTimeline request.timeline
    }

toMemoCreatedPayload :: MemoPayloadRequest -> MemoCreatedPayload
toMemoCreatedPayload request =
  MemoCreatedPayload
    { identifier = request.identifier,
      title = request.title,
      slug = request.slug,
      entries = map toMemoEntry request.entries,
      tags = request.tags,
      status = request.status,
      timeline = toTimeline request.timeline
    }

parsePayload :: (FromJSON a) => Value -> Either String a
parsePayload value = case parseMaybe parseJSON value of
  Nothing -> Left "Invalid Payload"
  Just v -> Right v

makeEvent :: EventRequest -> EventType -> EventPayload -> Event
makeEvent request eventType' payload' =
  Event
    { identifier = request.identifier,
      occurredAt = request.occurredAt,
      eventType = eventType',
      payload = payload'
    }

toDomain :: EventRequest -> Either String Event
toDomain request = case request.eventType of
  "article.created" -> do
    (wrapper :: SnapshotPayloadRequest ArticlePayloadRequest) <- parsePayload request.payload
    Right $ makeEvent request ArticleCreated (ArticleCreatedPayload' (toArticleCreatedPayload wrapper.snapshot))
  "article.edited" ->
    ( \payload' ->
        makeEvent
          request
          ArticleEdited
          ( ArticleEditedPayload'
              ArticleEditedPayload
                { next = toArticleCreatedPayload payload'.next,
                  before = toArticleCreatedPayload payload'.before
                }
          )
    )
      <$> (parsePayload request.payload :: Either String (EditedPayloadRequest ArticlePayloadRequest))
  "article.terminated" -> do
    (wrapper :: ArticleTerminatedPayloadRequest) <- parsePayload request.payload
    Right $ makeEvent request ArticleTerminated (ArticleTerminatePayload' wrapper.article)
  "memo.created" -> do
    (wrapper :: SnapshotPayloadRequest MemoPayloadRequest) <- parsePayload request.payload
    Right $ makeEvent request MemoCreated (MemoCreatedPayload' (toMemoCreatedPayload wrapper.snapshot))
  "memo.edited" ->
    ( \payload' ->
        makeEvent
          request
          MemoEdited
          ( MemoEditedPayload'
              MemoEditedPayload
                { next = toMemoCreatedPayload payload'.next,
                  before = toMemoCreatedPayload payload'.before
                }
          )
    )
      <$> (parsePayload request.payload :: Either String (EditedPayloadRequest MemoPayloadRequest))
  "memo.terminated" -> do
    (wrapper :: MemoTerminatedPayloadRequest) <- parsePayload request.payload
    Right $ makeEvent request MemoTerminated (MemoTerminatePayload' wrapper.memo)
  _ -> Left ("Unknown event type: " <> request.eventType)

newtype PubSubMessage = PubSubMessage
  { messageData :: String
  }

instance FromJSON PubSubMessage where
  parseJSON = withObject "PubSubMessage" $ \v ->
    PubSubMessage <$> v .: "data"

newtype PubSubPushRequest = PubSubPushRequest
  { message :: PubSubMessage
  }
  deriving (Generic)

instance FromJSON PubSubPushRequest

decodePushRequest :: PubSubPushRequest -> Either String EventRequest
decodePushRequest pushRequest =
  case B64.decode (BS.pack pushRequest.message.messageData) of
    Left _ -> Left "Invalid base64 in PubSub message data"
    Right decoded -> eitherDecodeStrict decoded
