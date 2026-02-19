module Support.Helper.Domain.Event
  ( createArticleCreatedEvent,
    createArticleEditedEvent,
    createArticleTerminateEvent,
    createMemoCreatedEvent,
    createMemoEditedEvent,
    createMemoTerminateEvent,
  )
where

import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Common (Timeline (..))
import Domain.Event
  ( ArticleCreatedPayload (..),
    ArticleEditedPayload (..),
    Event (..),
    EventPayload (..),
    EventType (..),
    MemoCreatedPayload (..),
    MemoEditedPayload (..),
    MemoEntry (..),
  )

seedToTime :: Int -> UTCTime
seedToTime seed =
  addUTCTime (secondsToNominalDiffTime (fromIntegral seed)) (posixSecondsToUTCTime 0)

seedToTimeline :: Int -> Timeline
seedToTimeline seed = Timeline (seedToTime seed) (seedToTime (seed + 1))

createEvent :: Int -> EventType -> EventPayload -> Event
createEvent seed eventType' payload' =
  Event
    { identifier = "evt-" <> show seed,
      occurredAt = seedToTime seed,
      eventType = eventType',
      payload = payload'
    }

createArticleCreatedPayload :: Int -> ArticleCreatedPayload
createArticleCreatedPayload seed =
  ArticleCreatedPayload
    { identifier = "article-" <> show seed,
      title = "title-" <> show seed,
      content = "content-" <> show seed,
      excerpt = "excerpt-" <> show seed,
      slug = "slug-" <> show seed,
      status = "published",
      tags = ["tag-" <> show seed],
      timeline = seedToTimeline seed
    }

createMemoCreatedPayload :: Int -> MemoCreatedPayload
createMemoCreatedPayload seed =
  MemoCreatedPayload
    { identifier = "memo-" <> show seed,
      title = "title-" <> show seed,
      slug = "slug-" <> show seed,
      entries = [MemoEntry {text = "entry-" <> show seed, createdAt = seedToTime seed}],
      tags = ["tag-" <> show seed],
      status = "published",
      timeline = seedToTimeline seed
    }

createArticleCreatedEvent :: Int -> Event
createArticleCreatedEvent seed =
  createEvent seed ArticleCreated (ArticleCreatedPayload' (createArticleCreatedPayload seed))

createArticleEditedEvent :: Int -> Event
createArticleEditedEvent seed =
  createEvent
    seed
    ArticleEdited
    ( ArticleEditedPayload'
        ArticleEditedPayload
          { next = createArticleCreatedPayload (seed + 1),
            before = createArticleCreatedPayload seed
          }
    )

createArticleTerminateEvent :: Int -> Event
createArticleTerminateEvent seed =
  createEvent seed ArticleTerminated (ArticleTerminatePayload' ("article-" <> show seed))

createMemoCreatedEvent :: Int -> Event
createMemoCreatedEvent seed =
  createEvent seed MemoCreated (MemoCreatedPayload' (createMemoCreatedPayload seed))

createMemoEditedEvent :: Int -> Event
createMemoEditedEvent seed =
  createEvent
    seed
    MemoEdited
    ( MemoEditedPayload'
        MemoEditedPayload
          { next = createMemoCreatedPayload (seed + 1),
            before = createMemoCreatedPayload seed
          }
    )

createMemoTerminateEvent :: Int -> Event
createMemoTerminateEvent seed =
  createEvent seed MemoTerminated (MemoTerminatePayload' ("memo-" <> show seed))
