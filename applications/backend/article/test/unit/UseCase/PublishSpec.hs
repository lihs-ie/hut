{-# LANGUAGE GADTs #-}

module UseCase.PublishSpec (run) where

import Control.Monad (forM_)
import Data.IORef
import Domain.Article (Article (..))
import Domain.Article.Common
import Domain.Article.Draft qualified as Draft
import Domain.Article.Private qualified as Private
import Domain.Article.Published qualified as Published
import Shared.Domain.Error (
    DomainError (..),
    createAggregateNotFound,
    createOperationNotAllowed,
    createServiceUnavailable,
    createUnexpectedError,
 )
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.Domain.Excerpt (newExcerpt)
import Shared.UseCase.Command qualified as Command
import TestSupport
import UseCase.Persistence
import UseCase.Publish qualified as Workflow
import UseCase.TestSupport (command, expectError)

run :: IO ()
run = do
    value <- right identifier
    initial <- right start
    available <- right confirmed
    proof <- right (Draft.proofread (timestamp 1) available initial)
    excerpt <- right (newExcerpt "Summary")
    ready <- right (Draft.prepareToPublish (timestamp 2) excerpt proof)
    published <- right (Published.publish (timestamp 3) ready)
    private <- right (Private.takeDown (timestamp 4) published)
    calls <- newIORef []
    loads <- newIORef []
    let save outcome context article events = case events of
            Events [Here (DomainEvent payload)] -> do
                modifyIORef' calls (<> [(context, article, payload)])
                pure outcome
            _ -> fail "expected exactly one Publish event"
        dependencies state outcome =
            Workflow.Dependencies
                ( \requested -> do
                    modifyIORef' loads (<> [requested])
                    pure (Right (Just (LoadedForPublication state (save outcome))))
                )
        success state = dependencies state (Right ())
        noSave = check "rejected operation never saved" . null =<< readIORef calls
    request <- command (Workflow.PublishPayload value)
    result <- Workflow.publish (success (Ready ready)) request >>= right
    check "loads requested identity once" . (== [value]) =<< readIORef loads
    event <- case result.events of
        Events [Here (DomainEvent payload)] -> pure payload
        _ -> fail "expected exactly one result event"
    check "event references article" (event == value)
    check "atomic save gets matching result, event and metadata"
        . (== [(commandContext request, result.article, event)])
        =<< readIORef calls
    check
        "preserves identity and all publication content"
        ( result.article.identifier == value
            && result.article.publication == Draft.publicationContent ready
        )
    check
        "preserves creation and updates modification"
        ( result.article.timeline.createdAt == timestamp 0
            && result.article.timeline.updatedAt == request.timestamp
        )
    check
        "publication date follows lifecycle rule"
        (result.article.publishedAt == request.timestamp)
    writeIORef calls []
    forM_ [Unvalidated initial, Proofreaded proof, Published published, Private private] $ \state -> do
        outcome <- Workflow.publish (success state) request
        check "invalid state rejected" $ case outcome of
            Left (OperationNotAllowed _) -> True
            _ -> False
        noSave
    future <- right (Ready <$> Private.resumePublication (timestamp 20) private)
    stale <- Workflow.publish (success future) request
    check "backward timestamp rejected" $ case stale of
        Left (InvariantViolation _) -> True
        _ -> False
    noSave
    missing <-
        Workflow.publish
            (Workflow.Dependencies (const (pure (Right Nothing))))
            request
    expectError
        "missing article"
        (createAggregateNotFound "Article" (articleIdentifierText value))
        missing
    let loadFailure = createServiceUnavailable "Article" "load failed"
    failedLoad <-
        Workflow.publish
            (Workflow.Dependencies (const (pure (Left loadFailure))))
            request
    expectError "load error preserved" loadFailure failedLoad
    other <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    wrongRequest <- command (Workflow.PublishPayload other)
    wrong <- Workflow.publish (success (Ready ready)) wrongRequest
    expectError
        "loaded identity mismatch rejected"
        (createUnexpectedError "Article" "loaded identity does not match request")
        wrong
    noSave
    forM_
        [ createOperationNotAllowed "Article" "concurrent update or deletion"
        , createOperationNotAllowed "Slug" "already in use"
        , createServiceUnavailable "Outbox" "commit failed"
        ]
        $ \err -> do
            writeIORef calls []
            failed <- Workflow.publish (dependencies (Ready ready) (Left err)) request
            expectError "commit error returned instead of success" err failed
            check "exactly one conditional save attempt" . (== 1) . length =<< readIORef calls

    resumed <- right (Private.resumePublication (timestamp 5) private)
    republished <- Workflow.publish (success (Ready resumed)) request >>= right
    check
        "republication renews publication date"
        ( republished.article.publishedAt == request.timestamp
            && republished.article.publishedAt /= published.publishedAt
            && republished.article.publication == published.publication
            && republished.article.timeline.createdAt == published.timeline.createdAt
        )
