module Unit.UseCase.EventLoggerSpec (spec) where

import Aspects.Log (LogEntry (..), LogLevel (..))
import Control.Monad.Writer (WriterT (runWriterT))
import Domain.Event (Event)
import Support.Helper.Domain.Event
  ( createArticleCreatedEvent,
    createArticleEditedEvent,
    createArticleTerminateEvent,
    createChapterCreatedEvent,
    createChapterEditedEvent,
    createChapterTerminateEvent,
    createEngagementRecordedEvent,
    createMemoCreatedEvent,
    createMemoEditedEvent,
    createMemoTerminateEvent,
    createPageViewRecordedEvent,
    createSearchRecordedEvent,
    createSeriesCreatedEvent,
    createSeriesEditedEvent,
    createSeriesTerminateEvent,
    createTagPersistedEvent,
    createTagTerminatedEvent,
    createUniqueVisitorRecordedEvent,
  )
import Test.Hspec
import UseCase.EventLogger (logEvent)

runLogEvent :: Event -> IO [LogEntry]
runLogEvent event = do
  (_, logs) <- runWriterT (logEvent event)
  pure logs

spec :: Spec
spec = do
  describe "logEvent" $ do
    context "ArticleCreated" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createArticleCreatedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

      it "includes event identifier in log" $ do
        logs <- runLogEvent (createArticleCreatedEvent 1)
        any (\entry -> "evt-1" `elem` words entry.message || "evt-1" `isSubstringOf` entry.message) logs
          `shouldBe` True

    context "ArticleEdited" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createArticleEditedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "ArticleTerminated" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createArticleTerminateEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "MemoCreated" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createMemoCreatedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "MemoEdited" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createMemoEditedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "MemoTerminated" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createMemoTerminateEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "SeriesCreated" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createSeriesCreatedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "SeriesEdited" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createSeriesEditedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "SeriesTerminated" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createSeriesTerminateEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "ChapterCreated" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createChapterCreatedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "ChapterEdited" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createChapterEditedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "ChapterTerminated" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createChapterTerminateEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "TagPersisted" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createTagPersistedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "TagTerminated" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createTagTerminatedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "PageViewRecorded" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createPageViewRecordedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "UniqueVisitorRecorded" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createUniqueVisitorRecordedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "EngagementRecorded" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createEngagementRecordedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

    context "SearchRecorded" $ do
      it "emits an INFO log entry" $ do
        logs <- runLogEvent (createSearchRecordedEvent 1)
        logs `shouldSatisfy` (not . null)
        all (\entry -> entry.level == Info) logs `shouldBe` True

isSubstringOf :: String -> String -> Bool
isSubstringOf needle haystack =
  any (needle `isPrefix`) (tails haystack)
  where
    isPrefix [] _ = True
    isPrefix _ [] = False
    isPrefix (x : xs) (y : ys) = x == y && isPrefix xs ys
    tails [] = [[]]
    tails xs@(_ : rest) = xs : tails rest
