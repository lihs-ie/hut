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
import Domain.Event (ArticleCreatedPayload (..), ArticleEditedPayload (..), ChapterCreatedPayload (..), ChapterEditedPayload (..), Event (..), EventPayload (ArticleCreatedPayload', ArticleEditedPayload', ArticleTerminatePayload', ChapterCreatedPayload', ChapterEditedPayload', ChapterTerminatePayload', MemoCreatedPayload', MemoEditedPayload', MemoTerminatePayload', SeriesCreatedPayload', SeriesEditedPayload', SeriesTerminatePayload'), EventType (ArticleCreated, ArticleEdited, ArticleTerminated, ChapterCreated, ChapterEdited, ChapterTerminated, MemoCreated, MemoEdited, MemoTerminated, SeriesCreated, SeriesEdited, SeriesTerminated), MemoCreatedPayload (..), MemoEditedPayload (..), MemoEntry (MemoEntry, createdAt, text), SeriesChapter (..), SeriesCreatedPayload (..), SeriesEditedPayload (..))
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

data SeriesChapterRequest = SeriesChapterRequest
  { title :: String,
    slug :: String,
    content :: String,
    timeline :: TimelineRequest
  }
  deriving (Generic)

instance FromJSON SeriesChapterRequest

data SeriesPayloadRequest = SeriesPayloadRequest
  { identifier :: String,
    title :: String,
    slug :: String,
    description :: Maybe String,
    tags :: [String],
    chapters :: [SeriesChapterRequest],
    timeline :: TimelineRequest
  }
  deriving (Generic)

instance FromJSON SeriesPayloadRequest

newtype SeriesTerminatedPayloadRequest = SeriesTerminatedPayloadRequest
  { series :: String
  }
  deriving (Generic)

instance FromJSON SeriesTerminatedPayloadRequest

data ChapterPayloadRequest = ChapterPayloadRequest
  { identifier :: String,
    title :: String,
    slug :: String,
    content :: String,
    timeline :: TimelineRequest
  }
  deriving (Generic)

instance FromJSON ChapterPayloadRequest

newtype ChapterTerminatedPayloadRequest = ChapterTerminatedPayloadRequest
  { chapter :: String
  }
  deriving (Generic)

instance FromJSON ChapterTerminatedPayloadRequest

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

toSeriesChapter :: SeriesChapterRequest -> SeriesChapter
toSeriesChapter request =
  SeriesChapter
    { title = request.title,
      slug = request.slug,
      content = request.content,
      timeline = toTimeline request.timeline
    }

toSeriesCreatedPayload :: SeriesPayloadRequest -> SeriesCreatedPayload
toSeriesCreatedPayload request =
  SeriesCreatedPayload
    { identifier = request.identifier,
      title = request.title,
      slug = request.slug,
      description = request.description,
      tags = request.tags,
      chapters = map toSeriesChapter request.chapters,
      timeline = toTimeline request.timeline
    }

toChapterCreatedPayload :: ChapterPayloadRequest -> ChapterCreatedPayload
toChapterCreatedPayload request =
  ChapterCreatedPayload
    { identifier = request.identifier,
      title = request.title,
      slug = request.slug,
      content = request.content,
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
  "series.created" -> do
    (wrapper :: SnapshotPayloadRequest SeriesPayloadRequest) <- parsePayload request.payload
    Right $ makeEvent request SeriesCreated (SeriesCreatedPayload' (toSeriesCreatedPayload wrapper.snapshot))
  "series.edited" ->
    ( \payload' ->
        makeEvent
          request
          SeriesEdited
          ( SeriesEditedPayload'
              SeriesEditedPayload
                { next = toSeriesCreatedPayload payload'.next,
                  before = toSeriesCreatedPayload payload'.before
                }
          )
    )
      <$> (parsePayload request.payload :: Either String (EditedPayloadRequest SeriesPayloadRequest))
  "series.terminated" -> do
    (wrapper :: SeriesTerminatedPayloadRequest) <- parsePayload request.payload
    Right $ makeEvent request SeriesTerminated (SeriesTerminatePayload' wrapper.series)
  "chapter.created" -> do
    (wrapper :: SnapshotPayloadRequest ChapterPayloadRequest) <- parsePayload request.payload
    Right $ makeEvent request ChapterCreated (ChapterCreatedPayload' (toChapterCreatedPayload wrapper.snapshot))
  "chapter.edited" ->
    ( \payload' ->
        makeEvent
          request
          ChapterEdited
          ( ChapterEditedPayload'
              ChapterEditedPayload
                { next = toChapterCreatedPayload payload'.next,
                  before = toChapterCreatedPayload payload'.before
                }
          )
    )
      <$> (parsePayload request.payload :: Either String (EditedPayloadRequest ChapterPayloadRequest))
  "chapter.terminated" -> do
    (wrapper :: ChapterTerminatedPayloadRequest) <- parsePayload request.payload
    Right $ makeEvent request ChapterTerminated (ChapterTerminatePayload' wrapper.chapter)
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
