{-# LANGUAGE GADTs #-}

module UseCase.TakeDownSpec (run) where

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
import UseCase.LegacyPersistence
import UseCase.TakeDown qualified as Workflow
import UseCase.TestSupport (command, expectError)
import UseCase.TransactionSupport qualified as Tx

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
            _ -> fail "expected exactly one TakeDown event"
        dependencies state outcome =
            Tx.takeDownDependencies
                ( \requested -> do
                    modifyIORef' loads (<> [requested])
                    pure (Right (Just (LoadedForTakeDown state (save outcome))))
                )
        success state = dependencies state (Right ())
        noPersist = check "rejected operation never saved" . null =<< readIORef calls
    request <- command (Workflow.TakeDownPayload value)
    result <- Workflow.takeDown (success (Published published)) request >>= right
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
            && result.article.publication == published.publication
        )
    check
        "preserves creation and updates modification"
        ( result.article.timeline.createdAt == timestamp 0
            && result.article.timeline.updatedAt == request.timestamp
        )
    check
        "publication date follows lifecycle rule"
        (result.article.publishedAt == published.publishedAt)
    writeIORef calls []
    forM_ [Unvalidated initial, Proofreaded proof, Ready ready, Private private] $ \state -> do
        outcome <- Workflow.takeDown (success state) request
        check "invalid state rejected" $ case outcome of
            Left (OperationNotAllowed _) -> True
            _ -> False
        noPersist
    future <- right (Published <$> Published.publish (timestamp 20) ready)
    stale <- Workflow.takeDown (success future) request
    check "backward timestamp rejected" $ case stale of
        Left (InvariantViolation _) -> True
        _ -> False
    noPersist
    missing <-
        Workflow.takeDown
            (Tx.takeDownDependencies (const (pure (Right Nothing))))
            request
    expectError
        "missing article"
        (createAggregateNotFound "Article" (articleIdentifierText value))
        missing
    let loadFailure = createServiceUnavailable "Article" "load failed"
    failedLoad <-
        Workflow.takeDown
            (Tx.takeDownDependencies (const (pure (Left loadFailure))))
            request
    expectError "load error preserved" loadFailure failedLoad
    other <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    wrongRequest <- command (Workflow.TakeDownPayload other)
    wrong <- Workflow.takeDown (success (Published published)) wrongRequest
    expectError
        "loaded identity mismatch rejected"
        (createUnexpectedError "Article" "loaded identity does not match request")
        wrong
    noPersist
    forM_
        [ createOperationNotAllowed "Article" "concurrent update or deletion"
        , createOperationNotAllowed "Slug" "already in use"
        , createServiceUnavailable "Outbox" "commit failed"
        ]
        $ \err -> do
            writeIORef calls []
            failed <- Workflow.takeDown (dependencies (Published published) (Left err)) request
            expectError "commit error returned instead of success" err failed
            check "exactly one conditional save attempt" . (== 1) . length =<< readIORef calls
