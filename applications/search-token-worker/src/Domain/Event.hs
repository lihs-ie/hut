{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Domain.Event
  ( EventType (..),
    Event (..),
    EventPayload (..),
    ArticleCreatedPayload (..),
    ArticleEditedPayload (..),
    MemoEntry (..),
    MemoCreatedPayload (..),
    MemoEditedPayload (..),
    SeriesChapter (..),
    SeriesCreatedPayload (..),
    SeriesEditedPayload (..),
  )
where

import Data.Time (UTCTime)
import Domain.Common (Timeline)

data EventType
  = MemoCreated
  | MemoEdited
  | MemoTerminated
  | ArticleCreated
  | ArticleEdited
  | ArticleTerminated
  | TagPersisted
  | TagTerminated
  | SeriesCreated
  | SeriesEdited
  | SeriesTerminated
  deriving (Eq, Show, Bounded, Enum)

data ArticleCreatedPayload = ArticleCreatedPayload
  { identifier :: String,
    title :: String,
    content :: String,
    excerpt :: String,
    slug :: String,
    status :: String,
    tags :: [String],
    timeline :: Timeline
  }
  deriving (Eq, Show)

data ArticleEditedPayload = ArticleEditedPayload
  { next :: ArticleCreatedPayload,
    before :: ArticleCreatedPayload
  }
  deriving (Eq, Show)

type ArticleTerminatePayload = String

data MemoEntry = MemoEntry
  { text :: String,
    createdAt :: UTCTime
  }
  deriving (Eq, Show)

data MemoCreatedPayload = MemoCreatedPayload
  { identifier :: String,
    title :: String,
    slug :: String,
    entries :: [MemoEntry],
    tags :: [String],
    status :: String,
    timeline :: Timeline
  }
  deriving (Eq, Show)

data MemoEditedPayload = MemoEditedPayload
  { next :: MemoCreatedPayload,
    before :: MemoCreatedPayload
  }
  deriving (Eq, Show)

type MemoTerminatePayload = String

data SeriesChapter = SeriesChapter
  { title :: String,
    slug :: String,
    content :: String,
    timeline :: Timeline
  }
  deriving (Eq, Show)

data SeriesCreatedPayload = SeriesCreatedPayload
  { identifier :: String,
    title :: String,
    slug :: String,
    description :: Maybe String,
    tags :: [String],
    chapters :: [SeriesChapter],
    timeline :: Timeline
  }
  deriving (Eq, Show)

data SeriesEditedPayload = SeriesEditedPayload
  { next :: SeriesCreatedPayload,
    before :: SeriesCreatedPayload
  }
  deriving (Eq, Show)

type SeriesTerminatePayload = String

data EventPayload
  = ArticleCreatedPayload' ArticleCreatedPayload
  | ArticleEditedPayload' ArticleEditedPayload
  | ArticleTerminatePayload' ArticleTerminatePayload
  | MemoCreatedPayload' MemoCreatedPayload
  | MemoEditedPayload' MemoEditedPayload
  | MemoTerminatePayload' MemoTerminatePayload
  | SeriesCreatedPayload' SeriesCreatedPayload
  | SeriesEditedPayload' SeriesEditedPayload
  | SeriesTerminatePayload' SeriesTerminatePayload
  deriving (Eq, Show)

data Event = Event
  { identifier :: String,
    occurredAt :: UTCTime,
    eventType :: EventType,
    payload :: EventPayload
  }
  deriving (Eq, Show)
