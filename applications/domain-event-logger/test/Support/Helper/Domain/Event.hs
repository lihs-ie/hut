module Support.Helper.Domain.Event
  ( createArticleCreatedEvent,
    createArticleEditedEvent,
    createArticleTerminateEvent,
    createMemoCreatedEvent,
    createMemoEditedEvent,
    createMemoTerminateEvent,
    createSeriesCreatedEvent,
    createSeriesEditedEvent,
    createSeriesTerminateEvent,
    createChapterCreatedEvent,
    createChapterEditedEvent,
    createChapterTerminateEvent,
    createTagPersistedEvent,
    createTagTerminatedEvent,
    createPageViewRecordedEvent,
    createUniqueVisitorRecordedEvent,
    createEngagementRecordedEvent,
    createSearchRecordedEvent,
  )
where

import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Common (Timeline (..))
import Domain.Event
  ( ArticleCreatedPayload (..),
    ArticleEditedPayload (..),
    ChapterCreatedPayload (..),
    ChapterEditedPayload (..),
    EngagementRecordedPayload (..),
    Event (..),
    EventPayload (..),
    EventType (..),
    MemoCreatedPayload (..),
    MemoEditedPayload (..),
    MemoEntry (..),
    PageViewRecordedPayload (..),
    SearchRecordedPayload (..),
    SeriesChapter (..),
    SeriesCreatedPayload (..),
    SeriesEditedPayload (..),
    TagPersistedPayload (..),
    TagTerminatedPayload (..),
    UniqueVisitorRecordedPayload (..),
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

createSeriesCreatedPayload :: Int -> SeriesCreatedPayload
createSeriesCreatedPayload seed =
  SeriesCreatedPayload
    { identifier = "series-" <> show seed,
      title = "title-" <> show seed,
      slug = "slug-" <> show seed,
      description = Just ("description-" <> show seed),
      tags = ["tag-" <> show seed],
      chapters =
        [ SeriesChapter
            { title = "chapter-" <> show seed,
              slug = "chapter-slug-" <> show seed,
              content = "chapter-content-" <> show seed,
              timeline = seedToTimeline seed
            }
        ],
      timeline = seedToTimeline seed
    }

createChapterCreatedPayload :: Int -> ChapterCreatedPayload
createChapterCreatedPayload seed =
  ChapterCreatedPayload
    { identifier = "chapter-" <> show seed,
      title = "title-" <> show seed,
      slug = "slug-" <> show seed,
      content = "content-" <> show seed,
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

createSeriesCreatedEvent :: Int -> Event
createSeriesCreatedEvent seed =
  createEvent seed SeriesCreated (SeriesCreatedPayload' (createSeriesCreatedPayload seed))

createSeriesEditedEvent :: Int -> Event
createSeriesEditedEvent seed =
  createEvent
    seed
    SeriesEdited
    ( SeriesEditedPayload'
        SeriesEditedPayload
          { next = createSeriesCreatedPayload (seed + 1),
            before = createSeriesCreatedPayload seed
          }
    )

createSeriesTerminateEvent :: Int -> Event
createSeriesTerminateEvent seed =
  createEvent seed SeriesTerminated (SeriesTerminatePayload' ("series-" <> show seed))

createChapterCreatedEvent :: Int -> Event
createChapterCreatedEvent seed =
  createEvent seed ChapterCreated (ChapterCreatedPayload' (createChapterCreatedPayload seed))

createChapterEditedEvent :: Int -> Event
createChapterEditedEvent seed =
  createEvent
    seed
    ChapterEdited
    ( ChapterEditedPayload'
        ChapterEditedPayload
          { next = createChapterCreatedPayload (seed + 1),
            before = createChapterCreatedPayload seed
          }
    )

createChapterTerminateEvent :: Int -> Event
createChapterTerminateEvent seed =
  createEvent seed ChapterTerminated (ChapterTerminatePayload' ("chapter-" <> show seed))

createTagPersistedEvent :: Int -> Event
createTagPersistedEvent seed =
  createEvent
    seed
    TagPersisted
    ( TagPersistedPayload'
        TagPersistedPayload
          { identifier = "tag-" <> show seed,
            name = "tag-name-" <> show seed
          }
    )

createTagTerminatedEvent :: Int -> Event
createTagTerminatedEvent seed =
  createEvent
    seed
    TagTerminated
    ( TagTerminatedPayload'
        TagTerminatedPayload
          { tag = "tag-" <> show seed
          }
    )

createPageViewRecordedEvent :: Int -> Event
createPageViewRecordedEvent seed =
  createEvent
    seed
    PageViewRecorded
    ( PageViewRecordedPayload'
        PageViewRecordedPayload
          { path = "/articles/" <> show seed,
            count = seed
          }
    )

createUniqueVisitorRecordedEvent :: Int -> Event
createUniqueVisitorRecordedEvent seed =
  createEvent
    seed
    UniqueVisitorRecorded
    ( UniqueVisitorRecordedPayload'
        UniqueVisitorRecordedPayload
          { path = "/articles/" <> show seed,
            count = seed
          }
    )

createEngagementRecordedEvent :: Int -> Event
createEngagementRecordedEvent seed =
  createEvent
    seed
    EngagementRecorded
    ( EngagementRecordedPayload'
        EngagementRecordedPayload
          { path = "/articles/" <> show seed,
            duration = seed * 10
          }
    )

createSearchRecordedEvent :: Int -> Event
createSearchRecordedEvent seed =
  createEvent
    seed
    SearchRecorded
    ( SearchRecordedPayload'
        SearchRecordedPayload
          { query = "query-" <> show seed,
            count = seed
          }
    )
