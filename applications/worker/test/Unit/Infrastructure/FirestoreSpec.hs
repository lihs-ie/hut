module Unit.Infrastructure.FirestoreSpec (spec) where

import Control.Monad.Writer (runWriterT)
import Data.Either (isRight)
import Data.Time (addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Common (Timeline (..))
import Domain.SearchToken (ContentType (..), SearchToken (..), SearchTokenError (..))
import Gogol (Env)
import Gogol.FireStore (CloudPlatform'FullControl)
import Infrastructure.Firestore (createPersist, createTerminate)
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
