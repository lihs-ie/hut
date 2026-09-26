module UseCase.Transaction.ProofreadSpec (run) where

import Data.IORef
import Data.Set qualified as Set
import Domain.Article (Article (..))
import Domain.Article.Common
import Domain.Article.Draft qualified as Draft
import Shared.Domain.Error (DomainError (..))
import TestSupport
import UseCase.Proofread qualified as Proof
import UseCase.TestSupport (command)
import UseCase.Transaction.Fixture

run :: IO ()
run = do
    value <- right identifier
    initial <- right start
    reference <- right image
    request <- command (Proof.ProofreadPayload value)
    changedContent <- right (newDraftContent extractImages (DraftInput "Changed" "No images" (Just "changed") []))
    changed <- right (Draft.amendDraft (timestamp 2) changedContent initial)
    fixture <- newFixture (Just (Unvalidated initial)) NoFailure
    let media _ = do
            check "Media outside transaction" . not =<< readIORef fixture.active
            modifyIORef' fixture.trace (<> ["media"])
            writeIORef fixture.stored (Store (Just (Unvalidated changed)) [])
            pure (Right (Set.singleton reference))
    outcome <- Proof.proofread (Proof.Dependencies fixture.manager findArticle persistArticle (appendEvents "proofreaded") media) request
    check "changed images rejected" $ case outcome of
        Left (OperationNotAllowed _) -> True
        _ -> False
    check "concurrent draft preserved" . (== Store (Just (Unvalidated changed)) []) =<< readIORef fixture.stored
    check "recheck occurs in second boundary"
        . (== ["begin", "find", "commit", "media", "begin", "find", "rollback"])
        =<< readIORef fixture.trace

    -- Same images are not enough to reuse the old draft: validate NEW content.
    incompleteContent <- right (newDraftContent extractImages input{slug = Nothing})
    incomplete <- right (Draft.amendDraft (timestamp 2) incompleteContent initial)
    stale <- newFixture (Just (Unvalidated initial)) NoFailure
    let update _ = do
            writeIORef stale.stored (Store (Just (Unvalidated incomplete)) [])
            pure (Right (Set.singleton reference))
    invalid <- Proof.proofread (Proof.Dependencies stale.manager findArticle persistArticle (appendEvents "proofreaded") update) request
    check "current mandatory fields validated" $ case invalid of
        Left (InvariantViolation _) -> True
        _ -> False
    check "incomplete current draft preserved" . (== Store (Just (Unvalidated incomplete)) []) =<< readIORef stale.stored

    editedContent <- right (newDraftContent extractImages input{title = "New title"})
    edited <- right (Draft.amendDraft (timestamp 2) editedContent initial)
    fresh <- newFixture (Just (Unvalidated initial)) NoFailure
    let edit _ = do
            writeIORef fresh.stored (Store (Just (Unvalidated edited)) [])
            pure (Right (Set.singleton reference))
    result <- Proof.proofread (Proof.Dependencies fresh.manager findArticle persistArticle (appendEvents "proofreaded") edit) request >>= right
    check "current content used" (titleText (Draft.proofreadedContent result.article).title == "New title")

    removed <- newFixture (Just (Unvalidated initial)) NoFailure
    let remove _ = writeIORef removed.stored (Store Nothing []) >> pure (Right (Set.singleton reference))
    missing <- Proof.proofread (Proof.Dependencies removed.manager findArticle persistArticle (appendEvents "proofreaded") remove) request
    check "deleted article never recreated" $ case missing of
        Left (AggregateNotFound _) -> True
        _ -> False
