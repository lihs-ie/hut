{-# LANGUAGE GADTs #-}

module UseCase.DiscardArticleSpec (run) where

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
import TestSupport
import UseCase.LegacyPersistence
import UseCase.TestSupport (command, expectError)
import UseCase.TransactionSupport qualified as Tx

import UseCase.DiscardArticle qualified as Workflow

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

    let commit outcome context events = case events of
            Events [Here (DomainEvent payload)] -> do
                modifyIORef' calls (<> [(context, payload)])
                pure outcome
            _ -> fail "expected exactly one ArticleDiscarded"
        dependencies state outcome =
            Tx.discardArticleDependencies
                ( \requested -> do
                    modifyIORef' loads (<> [requested])
                    pure (Right (Just (LoadedForDiscard state (commit outcome))))
                )
        success state = dependencies state (Right ())
        noCommit = check "rejected operation not committed" . null =<< readIORef calls
    request <- command (Workflow.DiscardArticlePayload value)
    forM_ [Unvalidated initial, Proofreaded proof, Ready ready, Private private] $ \state -> do
        writeIORef calls []
        writeIORef loads []
        result <- Workflow.discardArticle (success state) request >>= right
        check "loads once" . (== [value]) =<< readIORef loads
        event <- case result.events of
            Events [Here (DomainEvent payload)] -> pure payload
            _ -> fail "expected one ArticleDiscarded result event"
        check
            "result and event reference discarded article"
            (result.article == value && event == value)
        check "one atomic discard with command metadata and event"
            . (== [(commandContext request, value)])
            =<< readIORef calls
    writeIORef calls []
    denied <- Workflow.discardArticle (success (Published published)) request
    check "published cannot be discarded" $ case denied of
        Left (OperationNotAllowed _) -> True
        _ -> False
    noCommit
    missing <-
        Workflow.discardArticle
            (Tx.discardArticleDependencies (const (pure (Right Nothing))))
            request
    expectError
        "missing article"
        (createAggregateNotFound "Article" (articleIdentifierText value))
        missing
    let loadFailure = createServiceUnavailable "Article" "load failed"
    failedLoad <-
        Workflow.discardArticle
            (Tx.discardArticleDependencies (const (pure (Left loadFailure))))
            request
    expectError "load error propagated" loadFailure failedLoad
    other <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    wrongRequest <- command (Workflow.DiscardArticlePayload other)
    wrong <- Workflow.discardArticle (success (Private private)) wrongRequest
    expectError
        "identity mismatch"
        (createUnexpectedError "Article" "loaded identity does not match request")
        wrong
    noCommit
    forM_
        [ createOperationNotAllowed "Article" "concurrent publication"
        , createServiceUnavailable "Outbox" "commit failed"
        ]
        $ \err -> do
            writeIORef calls []
            failed <- Workflow.discardArticle (dependencies (Private private) (Left err)) request
            expectError "failed commit returns no successful result" err failed
            check "single commit attempt" . (== 1) . length =<< readIORef calls
