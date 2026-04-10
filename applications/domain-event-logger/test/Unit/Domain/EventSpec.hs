module Unit.Domain.EventSpec (spec) where

import Control.Monad (forM_)
import Data.Time (getCurrentTime)
import Domain.Common (Timeline (..))
import Domain.Event
  ( ArticleCreatedPayload (..),
    ArticleEditedPayload (..),
    ChapterCreatedPayload (..),
    ChapterEditedPayload (..),
    Event (..),
    EventPayload (..),
    EventType (..),
    MemoCreatedPayload (..),
    MemoEditedPayload (..),
    MemoEntry (..),
    PageViewRecordedPayload (..),
    SeriesChapter (..),
    SeriesCreatedPayload (..),
    SeriesEditedPayload (..),
    TagPersistedPayload (..),
    TagTerminatedPayload (..),
    UniqueVisitorRecordedPayload (..),
    EngagementRecordedPayload (..),
    SearchRecordedPayload (..),
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "EventType" $ do
    context "show" $ do
      forM_
        [ (MemoCreated, "MemoCreated"),
          (MemoEdited, "MemoEdited"),
          (MemoTerminated, "MemoTerminated"),
          (ArticleCreated, "ArticleCreated"),
          (ArticleEdited, "ArticleEdited"),
          (ArticleTerminated, "ArticleTerminated"),
          (TagPersisted, "TagPersisted"),
          (TagTerminated, "TagTerminated"),
          (SeriesCreated, "SeriesCreated"),
          (SeriesEdited, "SeriesEdited"),
          (SeriesTerminated, "SeriesTerminated"),
          (ChapterCreated, "ChapterCreated"),
          (ChapterEdited, "ChapterEdited"),
          (ChapterTerminated, "ChapterTerminated"),
          (PageViewRecorded, "PageViewRecorded"),
          (UniqueVisitorRecorded, "UniqueVisitorRecorded"),
          (EngagementRecorded, "EngagementRecorded"),
          (SearchRecorded, "SearchRecorded")
        ]
        $ \(eventType, expected) ->
          it ("returns " <> expected) $ do
            show eventType `shouldBe` expected

    context "Bounded" $ do
      it "has all 18 expected constructors" $ do
        let allValues = [minBound .. maxBound] :: [EventType]
        length allValues `shouldBe` 18
        allValues
          `shouldMatchList` [ MemoCreated,
                              MemoEdited,
                              MemoTerminated,
                              ArticleCreated,
                              ArticleEdited,
                              ArticleTerminated,
                              TagPersisted,
                              TagTerminated,
                              SeriesCreated,
                              SeriesEdited,
                              SeriesTerminated,
                              ChapterCreated,
                              ChapterEdited,
                              ChapterTerminated,
                              PageViewRecorded,
                              UniqueVisitorRecorded,
                              EngagementRecorded,
                              SearchRecorded
                            ]

    context "Eq" $ do
      it "returns true for same constructors" $ do
        ArticleCreated `shouldBe` ArticleCreated

      it "returns false for different constructors" $ do
        ArticleCreated `shouldNotBe` MemoCreated

  describe "ArticleCreatedPayload" $ do
    it "holds all fields" $ do
      now <- getCurrentTime
      let timeline' = Timeline now now
          actual =
            ArticleCreatedPayload
              { identifier = "article-1",
                title = "Title",
                content = "Content",
                excerpt = "Excerpt",
                slug = "title",
                status = "published",
                tags = ["haskell", "test"],
                timeline = timeline'
              }
      actual.identifier `shouldBe` "article-1"
      actual.title `shouldBe` "Title"
      actual.content `shouldBe` "Content"
      actual.excerpt `shouldBe` "Excerpt"
      actual.slug `shouldBe` "title"
      actual.status `shouldBe` "published"
      actual.tags `shouldBe` ["haskell", "test"]
      actual.timeline `shouldBe` timeline'

  describe "ArticleEditedPayload" $ do
    it "holds next and before" $ do
      now <- getCurrentTime
      let timeline' = Timeline now now
          before' =
            ArticleCreatedPayload
              { identifier = "article-1",
                title = "Old",
                content = "Old Content",
                excerpt = "Old Excerpt",
                slug = "old",
                status = "draft",
                tags = [],
                timeline = timeline'
              }
          next' =
            ArticleCreatedPayload
              { identifier = "article-1",
                title = "New",
                content = "New Content",
                excerpt = "New Excerpt",
                slug = "new",
                status = "published",
                tags = ["updated"],
                timeline = timeline'
              }
          actual = ArticleEditedPayload {next = next', before = before'}
      actual.next `shouldBe` next'
      actual.before `shouldBe` before'

  describe "MemoEntry" $ do
    it "holds text and createdAt" $ do
      now <- getCurrentTime
      let actual = MemoEntry {text = "entry text", createdAt = now}
      actual.text `shouldBe` "entry text"
      actual.createdAt `shouldBe` now

  describe "MemoCreatedPayload" $ do
    it "holds all fields" $ do
      now <- getCurrentTime
      let timeline' = Timeline now now
          entry = MemoEntry {text = "memo entry", createdAt = now}
          actual =
            MemoCreatedPayload
              { identifier = "memo-1",
                title = "Memo",
                slug = "memo",
                entries = [entry],
                tags = ["memo-tag"],
                status = "published",
                timeline = timeline'
              }
      actual.identifier `shouldBe` "memo-1"
      actual.title `shouldBe` "Memo"
      actual.slug `shouldBe` "memo"
      actual.entries `shouldBe` [entry]
      actual.tags `shouldBe` ["memo-tag"]
      actual.status `shouldBe` "published"
      actual.timeline `shouldBe` timeline'

  describe "MemoEditedPayload" $ do
    it "holds next and before" $ do
      now <- getCurrentTime
      let timeline' = Timeline now now
          entry = MemoEntry {text = "text", createdAt = now}
          before' =
            MemoCreatedPayload
              { identifier = "memo-1",
                title = "Old",
                slug = "old",
                entries = [entry],
                tags = [],
                status = "draft",
                timeline = timeline'
              }
          next' =
            MemoCreatedPayload
              { identifier = "memo-1",
                title = "New",
                slug = "new",
                entries = [entry],
                tags = ["updated"],
                status = "published",
                timeline = timeline'
              }
          actual = MemoEditedPayload {next = next', before = before'}
      actual.next `shouldBe` next'
      actual.before `shouldBe` before'

  describe "SeriesChapter" $ do
    it "holds title, slug, and content" $ do
      now <- getCurrentTime
      let timeline' = Timeline now now
          actual =
            SeriesChapter
              { title = "Chapter 1",
                slug = "chapter-1",
                content = "Chapter content",
                timeline = timeline'
              }
      actual.title `shouldBe` "Chapter 1"
      actual.slug `shouldBe` "chapter-1"
      actual.content `shouldBe` "Chapter content"
      actual.timeline `shouldBe` timeline'

  describe "SeriesCreatedPayload" $ do
    it "holds all fields" $ do
      now <- getCurrentTime
      let timeline' = Timeline now now
          chapter = SeriesChapter {title = "C1", slug = "c1", content = "content", timeline = timeline'}
          actual =
            SeriesCreatedPayload
              { identifier = "series-1",
                title = "My Series",
                slug = "my-series",
                description = Just "A description",
                tags = ["haskell"],
                chapters = [chapter],
                timeline = timeline'
              }
      actual.identifier `shouldBe` "series-1"
      actual.title `shouldBe` "My Series"
      actual.slug `shouldBe` "my-series"
      actual.description `shouldBe` Just "A description"
      actual.tags `shouldBe` ["haskell"]
      actual.chapters `shouldBe` [chapter]
      actual.timeline `shouldBe` timeline'

  describe "SeriesEditedPayload" $ do
    it "holds next and before" $ do
      now <- getCurrentTime
      let timeline' = Timeline now now
          before' =
            SeriesCreatedPayload
              { identifier = "series-1",
                title = "Old",
                slug = "old",
                description = Nothing,
                tags = [],
                chapters = [],
                timeline = timeline'
              }
          next' =
            SeriesCreatedPayload
              { identifier = "series-1",
                title = "New",
                slug = "new",
                description = Just "Updated",
                tags = ["updated"],
                chapters = [],
                timeline = timeline'
              }
          actual = SeriesEditedPayload {next = next', before = before'}
      actual.next `shouldBe` next'
      actual.before `shouldBe` before'

  describe "ChapterCreatedPayload" $ do
    it "holds all fields" $ do
      now <- getCurrentTime
      let timeline' = Timeline now now
          actual =
            ChapterCreatedPayload
              { identifier = "chapter-1",
                title = "Chapter Title",
                slug = "chapter-title",
                content = "Chapter content",
                timeline = timeline'
              }
      actual.identifier `shouldBe` "chapter-1"
      actual.title `shouldBe` "Chapter Title"
      actual.slug `shouldBe` "chapter-title"
      actual.content `shouldBe` "Chapter content"
      actual.timeline `shouldBe` timeline'

  describe "ChapterEditedPayload" $ do
    it "holds next and before" $ do
      now <- getCurrentTime
      let timeline' = Timeline now now
          before' =
            ChapterCreatedPayload
              { identifier = "chapter-1",
                title = "Old Title",
                slug = "old-title",
                content = "Old content",
                timeline = timeline'
              }
          next' =
            ChapterCreatedPayload
              { identifier = "chapter-1",
                title = "New Title",
                slug = "new-title",
                content = "New content",
                timeline = timeline'
              }
          actual = ChapterEditedPayload {next = next', before = before'}
      actual.next `shouldBe` next'
      actual.before `shouldBe` before'

  describe "TagPersistedPayload" $ do
    it "holds identifier and name" $ do
      let actual = TagPersistedPayload {identifier = "tag-1", name = "haskell"}
      actual.identifier `shouldBe` "tag-1"
      actual.name `shouldBe` "haskell"

  describe "TagTerminatedPayload" $ do
    it "holds tag identifier" $ do
      let actual = TagTerminatedPayload {tag = "tag-1"}
      actual.tag `shouldBe` "tag-1"

  describe "PageViewRecordedPayload" $ do
    it "holds path and count" $ do
      let actual = PageViewRecordedPayload {path = "/articles/1", count = 42}
      actual.path `shouldBe` "/articles/1"
      actual.count `shouldBe` 42

  describe "UniqueVisitorRecordedPayload" $ do
    it "holds path and count" $ do
      let actual = UniqueVisitorRecordedPayload {path = "/articles/1", count = 10}
      actual.path `shouldBe` "/articles/1"
      actual.count `shouldBe` 10

  describe "EngagementRecordedPayload" $ do
    it "holds path and duration" $ do
      let actual = EngagementRecordedPayload {path = "/articles/1", duration = 120}
      actual.path `shouldBe` "/articles/1"
      actual.duration `shouldBe` 120

  describe "SearchRecordedPayload" $ do
    it "holds query and count" $ do
      let actual = SearchRecordedPayload {query = "haskell", count = 5}
      actual.query `shouldBe` "haskell"
      actual.count `shouldBe` 5

  describe "EventPayload" $ do
    it "wraps ArticleCreatedPayload" $ do
      now <- getCurrentTime
      let timeline' = Timeline now now
          article =
            ArticleCreatedPayload
              { identifier = "a-1",
                title = "T",
                content = "C",
                excerpt = "E",
                slug = "s",
                status = "published",
                tags = [],
                timeline = timeline'
              }
          actual = ArticleCreatedPayload' article
      actual `shouldBe` ArticleCreatedPayload' article

    it "wraps ArticleTerminatePayload" $ do
      let actual = ArticleTerminatePayload' "ref-1"
      actual `shouldBe` ArticleTerminatePayload' "ref-1"

    it "wraps TagPersistedPayload" $ do
      let actual = TagPersistedPayload' (TagPersistedPayload {identifier = "tag-1", name = "haskell"})
      actual `shouldBe` TagPersistedPayload' (TagPersistedPayload {identifier = "tag-1", name = "haskell"})

    it "wraps TagTerminatedPayload" $ do
      let actual = TagTerminatedPayload' (TagTerminatedPayload {tag = "tag-1"})
      actual `shouldBe` TagTerminatedPayload' (TagTerminatedPayload {tag = "tag-1"})

    it "wraps PageViewRecordedPayload" $ do
      let actual = PageViewRecordedPayload' (PageViewRecordedPayload {path = "/", count = 1})
      actual `shouldBe` PageViewRecordedPayload' (PageViewRecordedPayload {path = "/", count = 1})

    it "wraps UniqueVisitorRecordedPayload" $ do
      let actual = UniqueVisitorRecordedPayload' (UniqueVisitorRecordedPayload {path = "/", count = 1})
      actual `shouldBe` UniqueVisitorRecordedPayload' (UniqueVisitorRecordedPayload {path = "/", count = 1})

    it "wraps EngagementRecordedPayload" $ do
      let actual = EngagementRecordedPayload' (EngagementRecordedPayload {path = "/", duration = 60})
      actual `shouldBe` EngagementRecordedPayload' (EngagementRecordedPayload {path = "/", duration = 60})

    it "wraps SearchRecordedPayload" $ do
      let actual = SearchRecordedPayload' (SearchRecordedPayload {query = "test", count = 3})
      actual `shouldBe` SearchRecordedPayload' (SearchRecordedPayload {query = "test", count = 3})

  describe "Event" $ do
    it "holds all fields" $ do
      now <- getCurrentTime
      let actual =
            Event
              { identifier = "evt-1",
                occurredAt = now,
                eventType = ArticleCreated,
                payload = ArticleTerminatePayload' "ref-1"
              }
      actual.identifier `shouldBe` "evt-1"
      actual.occurredAt `shouldBe` now
      actual.eventType `shouldBe` ArticleCreated
      actual.payload `shouldBe` ArticleTerminatePayload' "ref-1"
