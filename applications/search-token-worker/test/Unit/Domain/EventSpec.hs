module Unit.Domain.EventSpec (spec) where

import Control.Monad (forM_)
import Data.Time (getCurrentTime)
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
          (TagTerminated, "TagTerminated")
        ]
        $ \(eventType, expected) ->
          it ("returns " <> expected) $ do
            show eventType `shouldBe` expected

    context "Bounded" $ do
      it "has all expected constructors" $ do
        let allValues = [minBound .. maxBound] :: [EventType]
        allValues
          `shouldMatchList` [ MemoCreated,
                              MemoEdited,
                              MemoTerminated,
                              ArticleCreated,
                              ArticleEdited,
                              ArticleTerminated,
                              TagPersisted,
                              TagTerminated
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

    it "wraps MemoTerminatePayload" $ do
      let actual = MemoTerminatePayload' "ref-2"
      actual `shouldBe` MemoTerminatePayload' "ref-2"

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
