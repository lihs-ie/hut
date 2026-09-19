{-# LANGUAGE GADTs #-}

module UseCase.ProofreadSpec (run) where

import Control.Monad (forM_)
import Data.IORef
import Data.Set qualified as Set
import Domain.Article qualified as Article
import Domain.Article.Common
import Domain.Article.Draft qualified as Draft
import Domain.Article.Event (ProofreadedArticleContent (..))
import Domain.Article.Private qualified as Private
import Domain.Article.Published qualified as Published
import Shared.Domain.Error (
    DomainError (..),
    createAggregateNotFound,
    createInvariantViolation,
    createOperationNotAllowed,
    createServiceUnavailable,
    createUnexpectedError,
 )
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.Domain.Excerpt (newExcerpt)
import Shared.UseCase.Command qualified as Command
import TestSupport
import UseCase.Persistence
import UseCase.Proofread qualified as Proof
import UseCase.TestSupport (command, expectError)

run :: IO ()
run = do
    value <- right identifier
    initial <- right start
    available <- right confirmed
    reference <- right image
    proof <- right (Draft.proofread (timestamp 1) available initial)
    excerpt <- right (newExcerpt "Summary")
    ready <- right (Draft.prepareToPublish (timestamp 2) excerpt proof)
    published <- right (Published.publish (timestamp 3) ready)
    private <- right (Private.takeDown (timestamp 4) published)
    saved <- newIORef []
    checked <- newIORef []
    loaded <- newIORef []
    let persist outcome context article events = case events of
            Events [Here (DomainEvent payload)] -> do
                modifyIORef' saved (<> [(context, article, payload)])
                pure outcome
            _ -> fail "expected one ArticleProofreaded event"
        dependencies state availability outcome =
            Proof.Dependencies
                ( \requested -> do
                    modifyIORef' loaded (<> [requested])
                    pure (Right (Just (LoadedForProofreading state (persist outcome))))
                )
                (\requested -> modifyIORef' checked (<> [requested]) >> pure availability)
        success state = dependencies state (Right (Set.singleton reference)) (Right ())
    request <- command (Proof.ProofreadPayload value)
    result <- Proof.proofread (success (Article.Unvalidated initial)) request >>= right
    check "loads requested article once" . (== [value]) =<< readIORef loaded
    check "checks exact image set" . (== [Set.singleton reference]) =<< readIORef checked
    payload <- case result.events of
        Events [Here (DomainEvent event)] -> pure event
        _ -> fail "expected proofread event"
    check
        "event carries generation snapshot"
        ( payload.article == value
            && titleText payload.title == input.title
            && contentText payload.body == input.body
        )
    check "saved atomically with command metadata"
        . (== [(commandContext request, result.article, payload)])
        =<< readIORef saved
    check
        "proofreading updates timestamp and preserves creation"
        ( (Draft.draftTimeline result.article).createdAt == timestamp 0
            && (Draft.draftTimeline result.article).updatedAt == request.timestamp
        )
    writeIORef saved []
    writeIORef checked []
    noImagesContent <-
        right
            ( newDraftContent
                extractImages
                (DraftInput "Text" "No managed images" (Just "text") [])
            )
    noImages <- right (Draft.newUnvalidatedDraft value (timestamp 0) noImagesContent)
    noImagesResult <-
        Proof.proofread
            ( dependencies
                (Article.Unvalidated noImages)
                (Left (createServiceUnavailable "Media" "must not be called"))
                (Right ())
            )
            request
            >>= right
    check "image-free article needs no Media request" . null =<< readIORef checked
    check
        "image-free article proofread"
        (Set.null (Draft.proofreadedContent noImagesResult.article).images)
    forM_
        [ Article.Proofreaded proof
        , Article.Ready ready
        , Article.Published published
        , Article.Private private
        ]
        $ \state -> do
            writeIORef saved []
            writeIORef checked []
            outcome <- Proof.proofread (success state) request
            check "wrong states rejected" $ case outcome of
                Left (OperationNotAllowed _) -> True
                _ -> False
            check "wrong state not checked against Media" . null =<< readIORef checked
            check "wrong state not saved" . null =<< readIORef saved
    other <- right otherImage
    forM_ [Set.empty, Set.singleton other, Set.fromList [reference, other]] $ \availableImages -> do
        outcome <-
            Proof.proofread
                (dependencies (Article.Unvalidated initial) (Right availableImages) (Right ()))
                request
        expectError
            "unavailable or mismatched images rejected"
            (createInvariantViolation "Images" "all referenced images must be available")
            outcome
        check "bad image confirmation not saved" . null =<< readIORef saved
    let mediaFailure = createServiceUnavailable "Media" "unavailable"
    mediaResult <-
        Proof.proofread
            (dependencies (Article.Unvalidated initial) (Left mediaFailure) (Right ()))
            request
    expectError "Media failure propagated" mediaFailure mediaResult
    check "Media failure not saved" . null =<< readIORef saved
    forM_
        [ DraftInput "Idea" "" Nothing []
        , DraftInput "Idea" "Body" Nothing []
        ]
        $ \incomplete -> do
            content <- right (newDraftContent extractImages incomplete)
            draft <- right (Draft.newUnvalidatedDraft value (timestamp 0) content)
            outcome <- Proof.proofread (success (Article.Unvalidated draft)) request
            check "incomplete draft rejected" $ case outcome of
                Left (InvariantViolation _) -> True
                _ -> False
            check "incomplete not saved" . null =<< readIORef saved
    future <- right (Draft.amendDraft (timestamp 20) (Draft.draftContent initial) initial)
    stale <- Proof.proofread (success (Article.Unvalidated future)) request
    check "stale command rejected" $ case stale of
        Left (InvariantViolation _) -> True
        _ -> False
    check "stale command not saved" . null =<< readIORef saved
    missing <-
        Proof.proofread
            ( Proof.Dependencies
                (const (pure (Right Nothing)))
                (const (fail "unexpected Media query"))
            )
            request
    expectError "missing article" (createAggregateNotFound "Article" (articleIdentifierText value)) missing
    let loadFailure = createServiceUnavailable "Article" "load failed"
    failedLoad <-
        Proof.proofread
            ( Proof.Dependencies
                (const (pure (Left loadFailure)))
                (const (fail "unexpected Media query"))
            )
            request
    expectError "load failure" loadFailure failedLoad
    wrongValue <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    wrongRequest <- command (Proof.ProofreadPayload wrongValue)
    wrong <- Proof.proofread (success (Article.Unvalidated initial)) wrongRequest
    expectError
        "wrong identity"
        ( createUnexpectedError
            "Article"
            "loaded identity does not match request"
        )
        wrong
    forM_
        [ createOperationNotAllowed "Article" "concurrent update"
        , createServiceUnavailable "Outbox" "commit failed"
        ]
        $ \err -> do
            writeIORef saved []
            failed <-
                Proof.proofread
                    ( dependencies
                        (Article.Unvalidated initial)
                        (Right (Set.singleton reference))
                        (Left err)
                    )
                    request
            expectError "commit error" err failed
            check "single conditional commit" . (== 1) . length =<< readIORef saved
