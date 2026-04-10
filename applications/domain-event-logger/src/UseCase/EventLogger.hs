{-# LANGUAGE OverloadedStrings #-}

module UseCase.EventLogger
  ( logEvent,
  )
where

import Aspects.Log (LogEntry (LogEntry), LogLevel (Info))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Writer (MonadWriter (tell))
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Time (getCurrentTime)
import Domain.Event
  ( ArticleCreatedPayload (..),
    ArticleEditedPayload (..),
    ChapterCreatedPayload (..),
    ChapterEditedPayload (..),
    EngagementRecordedPayload (..),
    Event (..),
    EventPayload (..),
    MemoCreatedPayload (..),
    MemoEditedPayload (..),
    PageViewRecordedPayload (..),
    SearchRecordedPayload (..),
    SeriesCreatedPayload (..),
    SeriesEditedPayload (..),
    TagPersistedPayload (..),
    TagTerminatedPayload (..),
    UniqueVisitorRecordedPayload (..),
  )

logEvent :: (MonadIO m, MonadWriter [LogEntry] m) => Event -> m ()
logEvent event = do
  now <- liftIO getCurrentTime
  let summary = buildSummary event.payload
      message =
        LBS.unpack $
          encode $
            object
              [ "eventType" .= show event.eventType,
                "identifier" .= event.identifier,
                "occurredAt" .= show event.occurredAt,
                "summary" .= summary
              ]
  tell [LogEntry Info message now]

buildSummary :: EventPayload -> String
buildSummary payload = case payload of
  ArticleCreatedPayload' article ->
    "article identifier=" <> article.identifier <> " title=" <> article.title
  ArticleEditedPayload' edited ->
    "article identifier=" <> edited.next.identifier <> " title=" <> edited.next.title
  ArticleTerminatePayload' reference ->
    "article reference=" <> reference
  MemoCreatedPayload' memo ->
    "memo identifier=" <> memo.identifier <> " title=" <> memo.title
  MemoEditedPayload' edited ->
    "memo identifier=" <> edited.next.identifier <> " title=" <> edited.next.title
  MemoTerminatePayload' reference ->
    "memo reference=" <> reference
  SeriesCreatedPayload' series ->
    "series identifier=" <> series.identifier <> " title=" <> series.title
  SeriesEditedPayload' edited ->
    "series identifier=" <> edited.next.identifier <> " title=" <> edited.next.title
  SeriesTerminatePayload' reference ->
    "series reference=" <> reference
  ChapterCreatedPayload' chapter ->
    "chapter identifier=" <> chapter.identifier <> " title=" <> chapter.title
  ChapterEditedPayload' edited ->
    "chapter identifier=" <> edited.next.identifier <> " title=" <> edited.next.title
  ChapterTerminatePayload' reference ->
    "chapter reference=" <> reference
  TagPersistedPayload' tag ->
    "tag identifier=" <> tag.identifier <> " name=" <> tag.name
  TagTerminatedPayload' tag ->
    "tag reference=" <> tag.tag
  PageViewRecordedPayload' pv ->
    "pageView path=" <> pv.path <> " count=" <> show pv.count
  UniqueVisitorRecordedPayload' uv ->
    "uniqueVisitor path=" <> uv.path <> " count=" <> show uv.count
  EngagementRecordedPayload' eng ->
    "engagement path=" <> eng.path <> " duration=" <> show eng.duration
  SearchRecordedPayload' sr ->
    "search query=" <> sr.query <> " count=" <> show sr.count
