{-# LANGUAGE GADTs #-}

module UseCase.ResumePublicationSpec (run) where

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
import Shared.Domain.Event (Events (..))
import Shared.Domain.Excerpt (newExcerpt)
import Shared.UseCase.Command qualified as Command
import TestSupport
import UseCase.Persistence
import UseCase.TestSupport (command, expectError)

import UseCase.ResumePublication qualified as Workflow

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

    let save outcome context article (Events events) = do
            check "no events saved on resumption" (null events)
            modifyIORef' calls (<> [(context, article)])
            pure outcome
        dependencies state outcome =
            Workflow.Dependencies
                ( \requested -> do
                    modifyIORef' loads (<> [requested])
                    pure (Right (Just (LoadedForResumption state (save outcome))))
                )
        success state = dependencies state (Right ())
        noSave = check "rejected operation not saved" . null =<< readIORef calls
    request <- command (Workflow.ResumePublicationPayload value)
    result <- Workflow.resumePublication (success (Private private)) request >>= right
    check "load requested identity once" . (== [value]) =<< readIORef loads
    check "no result events" (null result.events.values)
    check
        "preserves full publication and identity"
        ( Draft.publicationContent result.article == private.publication
            && Draft.draftIdentifier result.article == value
        )
    check
        "creation preserved and modification updated"
        ( (Draft.draftTimeline result.article).createdAt == private.timeline.createdAt
            && (Draft.draftTimeline result.article).updatedAt == request.timestamp
        )
    check "one save with original metadata"
        . (== [(commandContext request, result.article)])
        =<< readIORef calls
    writeIORef calls []
    forM_ [Unvalidated initial, Proofreaded proof, Ready ready, Published published] $ \state -> do
        rejected <- Workflow.resumePublication (success state) request
        check "wrong state rejected" $ case rejected of
            Left (OperationNotAllowed _) -> True
            _ -> False
        noSave
    future <- right (Private.takeDown (timestamp 20) published)
    stale <- Workflow.resumePublication (success (Private future)) request
    check "backward timestamp rejected" $ case stale of
        Left (InvariantViolation _) -> True
        _ -> False
    noSave
    missing <-
        Workflow.resumePublication
            (Workflow.Dependencies (const (pure (Right Nothing))))
            request
    expectError
        "missing article"
        (createAggregateNotFound "Article" (articleIdentifierText value))
        missing
    let loadFailure = createServiceUnavailable "Article" "load failed"
    failedLoad <-
        Workflow.resumePublication
            (Workflow.Dependencies (const (pure (Left loadFailure))))
            request
    expectError "load error propagated" loadFailure failedLoad
    other <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    wrongRequest <- command (Workflow.ResumePublicationPayload other)
    wrong <- Workflow.resumePublication (success (Private private)) wrongRequest
    expectError
        "identity mismatch"
        (createUnexpectedError "Article" "loaded identity does not match request")
        wrong
    noSave
    forM_
        [ createOperationNotAllowed "Article" "concurrent update or deletion"
        , createServiceUnavailable "Article" "commit failed"
        ]
        $ \err -> do
            writeIORef calls []
            failed <- Workflow.resumePublication (dependencies (Private private) (Left err)) request
            expectError "commit error propagated" err failed
            check "single save attempt" . (== 1) . length =<< readIORef calls
