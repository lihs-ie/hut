module UseCase.ReadingSupport (states, published, failure, checkEmptyEvents, checkFailure) where

import Domain.Article (Article (..))
import Domain.Article.Draft qualified as Draft
import Domain.Article.Private qualified as Private
import Domain.Article.Published qualified as Published
import Shared.Domain.Error (DomainError, createServiceUnavailable)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Excerpt (newExcerpt)
import TestSupport

states :: IO [Article]
states = do
    initial <- right start
    available <- right confirmed
    proof <- right (Draft.proofread (timestamp 1) available initial)
    excerpt <- right (newExcerpt "Summary")
    ready <- right (Draft.prepareToPublish (timestamp 2) excerpt proof)
    public <- right (Published.publish (timestamp 3) ready)
    private <- right (Private.takeDown (timestamp 4) public)
    pure [Unvalidated initial, Proofreaded proof, Ready ready, Published public, Private private]
published :: IO Published.PublishedArticle
published = do
    articles <- states
    case [article | Published article <- articles] of
        [article] -> pure article
        _ -> fail "missing fixture"
failure :: DomainError
failure = createServiceUnavailable "Article" "read failed"
checkEmptyEvents :: Events '[] -> IO ()
checkEmptyEvents (Events values) = check "read has no events" (null values)
checkFailure :: Either DomainError a -> IO ()
checkFailure outcome = check "expected failure" $ case outcome of
    Left _ -> True
    Right _ -> False
