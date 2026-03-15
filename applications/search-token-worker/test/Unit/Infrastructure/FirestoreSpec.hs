module Unit.Infrastructure.FirestoreSpec (spec) where

import Control.Monad.Writer (runWriterT)
import Data.Either (isRight)
import Data.Time (addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Common (Timeline (..))
import Domain.SearchToken (ContentType (..), SearchToken (..), SearchTokenError (..))
import Gogol (Env)
import Gogol.FireStore (CloudPlatform'FullControl)
import Infrastructure.Firestore (createPersist, createTerminate, documentExists)
import Support.Helper.Infrastructure.Firestore (createEmulatorEnvironment, projectIdentifier, resetFirestore)
import Test.Hspec

type TestEnvironment = Env '[CloudPlatform'FullControl]

runPersist :: TestEnvironment -> [SearchToken] -> IO (Either SearchTokenError ())
runPersist environment tokens = do
  (result, _) <- runWriterT (createPersist environment projectIdentifier tokens)
  pure result

runTerminate :: TestEnvironment -> String -> IO (Either SearchTokenError ())
runTerminate environment reference = do
  (result, _) <- runWriterT (createTerminate environment projectIdentifier reference)
  pure result

createSearchToken :: Int -> SearchToken
createSearchToken seed =
  let baseTime = posixSecondsToUTCTime 0
      offset = secondsToNominalDiffTime (fromIntegral seed)
      seedTime = addUTCTime offset baseTime
   in SearchToken
        { identifier = "ngram:token-" <> show seed,
          reference = "article-" <> show seed,
          contentType = Article,
          value = "value-" <> show seed,
          timeline = Timeline seedTime seedTime
        }

createTagToken :: Int -> SearchToken
createTagToken seed =
  let baseTime = posixSecondsToUTCTime 0
      offset = secondsToNominalDiffTime (fromIntegral seed)
      seedTime = addUTCTime offset baseTime
   in SearchToken
        { identifier = "tag:tag-" <> show seed,
          reference = "article-" <> show seed,
          contentType = Article,
          value = "tag-" <> show seed,
          timeline = Timeline seedTime seedTime
        }

createTagTokenShared :: Int -> String -> SearchToken
createTagTokenShared seed tagIdentifier =
  let baseTime = posixSecondsToUTCTime 0
      offset = secondsToNominalDiffTime (fromIntegral seed)
      seedTime = addUTCTime offset baseTime
   in SearchToken
        { identifier = "tag:" <> tagIdentifier,
          reference = "article-" <> show seed,
          contentType = Article,
          value = tagIdentifier,
          timeline = Timeline seedTime seedTime
        }

checkDocumentExists :: TestEnvironment -> String -> IO Bool
checkDocumentExists environment path = do
  (result, _) <- runWriterT (documentExists environment path)
  pure result

spec :: Spec
spec = beforeAll createEmulatorEnvironment $ after_ resetFirestore $ do
  describe "createPersist" $ do
    context "successfully" $ do
      it "returns Right with valid tokens" $ \environment -> do
        let tokens = [createSearchToken 1, createSearchToken 2, createTagToken 1]
        result <- runPersist environment tokens
        result `shouldSatisfy` isRight

      it "returns Right with empty token list" $ \environment -> do
        result <- runPersist environment []
        result `shouldSatisfy` isRight

  describe "createTerminate" $ do
    context "successfully" $ do
      it "returns Right after persisting and terminating" $ \environment -> do
        let tokens = [createSearchToken 1, createSearchToken 2]
            reference = show Article <> ":" <> "article-1"
        persistResult <- runPersist environment tokens
        persistResult `shouldSatisfy` isRight
        terminateResult <- runTerminate environment reference
        terminateResult `shouldSatisfy` isRight

    context "failure" $ do
      it "returns Left NotFound for non-existent reference" $ \environment -> do
        result <- runTerminate environment "article:non-existent"
        result `shouldBe` Left NotFound

  describe "persist and terminate flow" $ do
    it "returns Left NotFound when terminating already terminated reference" $ \environment -> do
      let tokens = [createSearchToken 3, createTagToken 3]
          reference = show Article <> ":" <> "article-3"
      persistResult <- runPersist environment tokens
      persistResult `shouldSatisfy` isRight
      firstTerminate <- runTerminate environment reference
      firstTerminate `shouldSatisfy` isRight
      secondTerminate <- runTerminate environment reference
      secondTerminate `shouldBe` Left NotFound

  describe "shared tag behavior" $ do
    it "preserves shared token document when only one of two articles is terminated" $ \environment -> do
      let sharedTag = "shared-haskell"
          tokenArticle10 = createTagTokenShared 10 sharedTag
          tokenArticle11 = createTagTokenShared 11 sharedTag
          reference10 = show Article <> ":" <> "article-10"
      persistResult10 <- runPersist environment [tokenArticle10]
      persistResult10 `shouldSatisfy` isRight
      persistResult11 <- runPersist environment [tokenArticle11]
      persistResult11 `shouldSatisfy` isRight
      terminateResult <- runTerminate environment reference10
      terminateResult `shouldSatisfy` isRight
      let tokenDocPath = "projects/" <> projectIdentifier <> "/databases/(default)/documents/search-tokens/tag:" <> sharedTag
      tokenStillExists <- checkDocumentExists environment tokenDocPath
      tokenStillExists `shouldBe` True

    it "deletes token document when only article is terminated" $ \environment -> do
      let sharedTag = "only-haskell"
          tokenArticle20 = createTagTokenShared 20 sharedTag
          reference20 = show Article <> ":" <> "article-20"
      persistResult <- runPersist environment [tokenArticle20]
      persistResult `shouldSatisfy` isRight
      terminateResult <- runTerminate environment reference20
      terminateResult `shouldSatisfy` isRight
      let tokenDocPath = "projects/" <> projectIdentifier <> "/databases/(default)/documents/search-tokens/tag:" <> sharedTag
      tokenGone <- checkDocumentExists environment tokenDocPath
      tokenGone `shouldBe` False

    it "preserves remaining ref when one of two articles with shared tag is terminated" $ \environment -> do
      let sharedTag = "remaining-haskell"
          tokenArticle30 = createTagTokenShared 30 sharedTag
          tokenArticle31 = createTagTokenShared 31 sharedTag
          reference30 = show Article <> ":" <> "article-30"
          reference31 = show Article <> ":" <> "article-31"
      persistResult30 <- runPersist environment [tokenArticle30]
      persistResult30 `shouldSatisfy` isRight
      persistResult31 <- runPersist environment [tokenArticle31]
      persistResult31 `shouldSatisfy` isRight
      terminateResult <- runTerminate environment reference30
      terminateResult `shouldSatisfy` isRight
      let refDocPath = "projects/" <> projectIdentifier <> "/databases/(default)/documents/search-tokens/tag:" <> sharedTag <> "/refs/" <> reference31
      refStillExists <- checkDocumentExists environment refDocPath
      refStillExists `shouldBe` True
