module Unit.UseCase.EventHandlerSpec (spec) where

import Control.Monad.Writer (WriterT (runWriterT))
import Data.Either (isLeft, isRight)
import Data.IORef (newIORef, readIORef)
import Domain.Event (Event)
import Domain.SearchToken (ContentType (..), SearchToken (..), SearchTokenError (..))
import Support.Helper.Domain.Event
  ( createArticleCreatedEvent,
    createArticleEditedEvent,
    createArticleTerminateEvent,
    createChapterCreatedEvent,
    createChapterEditedEvent,
    createChapterTerminateEvent,
    createMemoCreatedEvent,
    createMemoEditedEvent,
    createMemoTerminateEvent,
    createSeriesCreatedEvent,
    createSeriesEditedEvent,
    createSeriesTerminateEvent,
  )
import Support.Mock.Domain.SearchToken (createMockPersist, createMockTerminateByReference)
import Test.Hspec
import UseCase.EventHandler (handle)

runHandle :: Either SearchTokenError () -> Either SearchTokenError () -> Event -> IO (Either SearchTokenError (), [SearchToken], [String])
runHandle persistResult terminateResult event = do
  persistRef <- newIORef []
  terminateRef <- newIORef []
  let mockPersist = createMockPersist persistRef persistResult
      mockTerminate = createMockTerminateByReference terminateRef terminateResult
  (result, _) <- runWriterT (handle mockPersist mockTerminate event)
  persisted <- readIORef persistRef
  terminated <- readIORef terminateRef
  pure (result, persisted, terminated)

spec :: Spec
spec = do
  describe "handle" $ do
    context "ArticleCreated" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createArticleCreatedEvent 1)
          result `shouldSatisfy` isRight

        it "calls persist with tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createArticleCreatedEvent 1)
          persisted `shouldSatisfy` (not . null)

        it "persists tokens with Article contentType" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createArticleCreatedEvent 1)
          all (\token -> token.contentType == Article) persisted `shouldBe` True

        it "persists tokens with correct reference" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createArticleCreatedEvent 1)
          all (\token -> token.reference == "article-1") persisted `shouldBe` True

        it "persists ngram tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createArticleCreatedEvent 1)
          any (\token -> take 6 token.identifier == "ngram:") persisted `shouldBe` True

        it "persists tag tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createArticleCreatedEvent 1)
          any (\token -> take 4 token.identifier == "tag:") persisted `shouldBe` True

        it "does not call terminate" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createArticleCreatedEvent 1)
          terminated `shouldBe` []

      context "failure" $ do
        it "returns Left when persist fails" $ do
          (result, _, _) <- runHandle (Left Unexpected) (Right ()) (createArticleCreatedEvent 1)
          result `shouldSatisfy` isLeft

    context "ArticleEdited" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createArticleEditedEvent 1)
          result `shouldSatisfy` isRight

        it "calls persist with tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createArticleEditedEvent 1)
          persisted `shouldSatisfy` (not . null)

        it "persists tokens with Article contentType" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createArticleEditedEvent 1)
          all (\token -> token.contentType == Article) persisted `shouldBe` True

        it "persists tokens with next payload reference" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createArticleEditedEvent 1)
          all (\token -> token.reference == "article-2") persisted `shouldBe` True

        it "does not call terminate" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createArticleEditedEvent 1)
          terminated `shouldBe` []

      context "failure" $ do
        it "returns Left when persist fails" $ do
          (result, _, _) <- runHandle (Left Unexpected) (Right ()) (createArticleEditedEvent 1)
          result `shouldSatisfy` isLeft

    context "ArticleTerminated" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createArticleTerminateEvent 1)
          result `shouldSatisfy` isRight

        it "calls terminate with correct reference" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createArticleTerminateEvent 1)
          terminated `shouldBe` ["article:article-1"]

        it "does not call persist" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createArticleTerminateEvent 1)
          persisted `shouldBe` []

      context "failure" $ do
        it "returns Left when terminate fails" $ do
          (result, _, _) <- runHandle (Right ()) (Left Unexpected) (createArticleTerminateEvent 1)
          result `shouldSatisfy` isLeft

    context "MemoCreated" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createMemoCreatedEvent 1)
          result `shouldSatisfy` isRight

        it "calls persist with tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createMemoCreatedEvent 1)
          persisted `shouldSatisfy` (not . null)

        it "persists tokens with Memo contentType" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createMemoCreatedEvent 1)
          all (\token -> token.contentType == Memo) persisted `shouldBe` True

        it "persists tokens with correct reference" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createMemoCreatedEvent 1)
          all (\token -> token.reference == "memo-1") persisted `shouldBe` True

        it "persists ngram tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createMemoCreatedEvent 1)
          any (\token -> take 6 token.identifier == "ngram:") persisted `shouldBe` True

        it "persists tag tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createMemoCreatedEvent 1)
          any (\token -> take 4 token.identifier == "tag:") persisted `shouldBe` True

        it "does not call terminate" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createMemoCreatedEvent 1)
          terminated `shouldBe` []

      context "failure" $ do
        it "returns Left when persist fails" $ do
          (result, _, _) <- runHandle (Left Unexpected) (Right ()) (createMemoCreatedEvent 1)
          result `shouldSatisfy` isLeft

    context "MemoEdited" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createMemoEditedEvent 1)
          result `shouldSatisfy` isRight

        it "calls persist with tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createMemoEditedEvent 1)
          persisted `shouldSatisfy` (not . null)

        it "persists tokens with Memo contentType" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createMemoEditedEvent 1)
          all (\token -> token.contentType == Memo) persisted `shouldBe` True

        it "persists tokens with next payload reference" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createMemoEditedEvent 1)
          all (\token -> token.reference == "memo-2") persisted `shouldBe` True

        it "does not call terminate" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createMemoEditedEvent 1)
          terminated `shouldBe` []

      context "failure" $ do
        it "returns Left when persist fails" $ do
          (result, _, _) <- runHandle (Left Unexpected) (Right ()) (createMemoEditedEvent 1)
          result `shouldSatisfy` isLeft

    context "MemoTerminated" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createMemoTerminateEvent 1)
          result `shouldSatisfy` isRight

        it "calls terminate with correct reference" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createMemoTerminateEvent 1)
          terminated `shouldBe` ["memo:memo-1"]

        it "does not call persist" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createMemoTerminateEvent 1)
          persisted `shouldBe` []

      context "failure" $ do
        it "returns Left when terminate fails" $ do
          (result, _, _) <- runHandle (Right ()) (Left Unexpected) (createMemoTerminateEvent 1)
          result `shouldSatisfy` isLeft

    context "SeriesCreated" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createSeriesCreatedEvent 1)
          result `shouldSatisfy` isRight

        it "calls persist with tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createSeriesCreatedEvent 1)
          persisted `shouldSatisfy` (not . null)

        it "persists tokens with Series contentType" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createSeriesCreatedEvent 1)
          all (\token -> token.contentType == Series) persisted `shouldBe` True

        it "persists tokens with correct reference" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createSeriesCreatedEvent 1)
          all (\token -> token.reference == "series-1") persisted `shouldBe` True

        it "persists ngram tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createSeriesCreatedEvent 1)
          any (\token -> take 6 token.identifier == "ngram:") persisted `shouldBe` True

        it "persists tag tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createSeriesCreatedEvent 1)
          any (\token -> take 4 token.identifier == "tag:") persisted `shouldBe` True

        it "does not call terminate" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createSeriesCreatedEvent 1)
          terminated `shouldBe` []

      context "failure" $ do
        it "returns Left when persist fails" $ do
          (result, _, _) <- runHandle (Left Unexpected) (Right ()) (createSeriesCreatedEvent 1)
          result `shouldSatisfy` isLeft

    context "SeriesEdited" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createSeriesEditedEvent 1)
          result `shouldSatisfy` isRight

        it "calls persist with tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createSeriesEditedEvent 1)
          persisted `shouldSatisfy` (not . null)

        it "persists tokens with Series contentType" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createSeriesEditedEvent 1)
          all (\token -> token.contentType == Series) persisted `shouldBe` True

        it "persists tokens with next payload reference" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createSeriesEditedEvent 1)
          all (\token -> token.reference == "series-2") persisted `shouldBe` True

        it "does not call terminate" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createSeriesEditedEvent 1)
          terminated `shouldBe` []

      context "failure" $ do
        it "returns Left when persist fails" $ do
          (result, _, _) <- runHandle (Left Unexpected) (Right ()) (createSeriesEditedEvent 1)
          result `shouldSatisfy` isLeft

    context "SeriesTerminated" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createSeriesTerminateEvent 1)
          result `shouldSatisfy` isRight

        it "calls terminate with correct reference" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createSeriesTerminateEvent 1)
          terminated `shouldBe` ["series:series-1"]

        it "does not call persist" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createSeriesTerminateEvent 1)
          persisted `shouldBe` []

      context "failure" $ do
        it "returns Left when terminate fails" $ do
          (result, _, _) <- runHandle (Right ()) (Left Unexpected) (createSeriesTerminateEvent 1)
          result `shouldSatisfy` isLeft

    context "ChapterCreated" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createChapterCreatedEvent 1)
          result `shouldSatisfy` isRight

        it "calls persist with tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createChapterCreatedEvent 1)
          persisted `shouldSatisfy` (not . null)

        it "persists tokens with Chapter contentType" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createChapterCreatedEvent 1)
          all (\token -> token.contentType == Chapter) persisted `shouldBe` True

        it "persists tokens with correct reference" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createChapterCreatedEvent 1)
          all (\token -> token.reference == "chapter-1") persisted `shouldBe` True

        it "persists ngram tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createChapterCreatedEvent 1)
          any (\token -> take 6 token.identifier == "ngram:") persisted `shouldBe` True

        it "does not call terminate" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createChapterCreatedEvent 1)
          terminated `shouldBe` []

      context "failure" $ do
        it "returns Left when persist fails" $ do
          (result, _, _) <- runHandle (Left Unexpected) (Right ()) (createChapterCreatedEvent 1)
          result `shouldSatisfy` isLeft

    context "ChapterEdited" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createChapterEditedEvent 1)
          result `shouldSatisfy` isRight

        it "calls persist with tokens" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createChapterEditedEvent 1)
          persisted `shouldSatisfy` (not . null)

        it "persists tokens with Chapter contentType" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createChapterEditedEvent 1)
          all (\token -> token.contentType == Chapter) persisted `shouldBe` True

        it "persists tokens with next payload reference" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createChapterEditedEvent 1)
          all (\token -> token.reference == "chapter-2") persisted `shouldBe` True

        it "does not call terminate" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createChapterEditedEvent 1)
          terminated `shouldBe` []

      context "failure" $ do
        it "returns Left when persist fails" $ do
          (result, _, _) <- runHandle (Left Unexpected) (Right ()) (createChapterEditedEvent 1)
          result `shouldSatisfy` isLeft

    context "ChapterTerminated" $ do
      context "successfully" $ do
        it "returns Right" $ do
          (result, _, _) <- runHandle (Right ()) (Right ()) (createChapterTerminateEvent 1)
          result `shouldSatisfy` isRight

        it "calls terminate with correct reference" $ do
          (_, _, terminated) <- runHandle (Right ()) (Right ()) (createChapterTerminateEvent 1)
          terminated `shouldBe` ["chapter:chapter-1"]

        it "does not call persist" $ do
          (_, persisted, _) <- runHandle (Right ()) (Right ()) (createChapterTerminateEvent 1)
          persisted `shouldBe` []

      context "failure" $ do
        it "returns Left when terminate fails" $ do
          (result, _, _) <- runHandle (Right ()) (Left Unexpected) (createChapterTerminateEvent 1)
          result `shouldSatisfy` isLeft
