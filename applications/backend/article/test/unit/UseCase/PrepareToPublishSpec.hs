{-# LANGUAGE GADTs #-}

module UseCase.PrepareToPublishSpec (run) where

import Control.Monad (forM_)
import Data.IORef
import Data.Text qualified as Text
import Domain.Article qualified as Article
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
import Shared.Domain.Excerpt (excerptText, newExcerpt)
import TestSupport
import UseCase.LegacyPersistence
import UseCase.PrepareToPublish qualified as Prepare
import UseCase.TestSupport (command, expectError)
import UseCase.TransactionSupport qualified as Tx

run :: IO ()
run = do
    context <- command ()
    value <- right identifier
    initial <- right start
    available <- right confirmed
    proof <- right (Draft.proofread (timestamp 1) available initial)
    excerpt <- right (newExcerpt "Original")
    ready <- right (Draft.prepareToPublish (timestamp 2) excerpt proof)
    published <- right (Published.publish (timestamp 3) ready)
    private <- right (Private.takeDown (timestamp 4) published)
    saved <- newIORef []
    loads <- newIORef ([] :: [(String, ArticleIdentifier)])
    let persist outcome context article events = do
            identities <- case events of
                Events [] -> pure []
                Events [Here (DomainEvent identity)] -> pure [identity]
                _ -> fail "unexpected event count"
            modifyIORef' saved (<> [(context, article, identities)])
            pure outcome
        loader label state outcome requested = do
            modifyIORef' loads (<> [(label, requested)])
            pure (Right (Just (LoadedForPreparation state (persist outcome))))
        dependencies state outcome =
            Tx.prepareToPublishDependencies
                context
                (loader "generation" state outcome)
                (loader "revision" state outcome)
    generated <- command (Prepare.ApplyGeneratedExcerpt value "Generated")
    result <-
        Tx.prepareToPublish
            (dependencies (Article.Proofreaded proof) (Right ()))
            generated
            >>= right
    check "generated uses revision-bound loader" . (== [("generation", value)]) =<< readIORef loads
    check
        "generated excerpt applied"
        ( excerptText (Draft.publicationContent result.article).excerpt == "Generated"
            && (Draft.draftTimeline result.article).updatedAt == timestamp 10
        )
    case result.events of
        Events [Here (DomainEvent identity)] -> check "ready event identity" (identity == value)
        _ -> fail "missing ready event"
    check "initial save and outbox one call"
        . (== [(commandContext generated, result.article, [value])])
        =<< readIORef saved
    writeIORef saved []
    writeIORef loads []
    revision <- command (Prepare.ReviseExcerpt value "Edited")
    revised <- Tx.prepareToPublish (dependencies (Article.Ready ready) (Right ())) revision >>= right
    check "manual edit uses latest-revision loader" . (== [("revision", value)]) =<< readIORef loads
    check "manual edit has no event" $ case revised.events of
        Events [] -> True
        _ -> False
    check "manual edit saves without outbox event"
        . (== [(commandContext revision, revised.article, [])])
        =<< readIORef saved
    check
        "manual edit preserves body slug and creation"
        ( (Draft.publicationContent revised.article).body == (Draft.publicationContent ready).body
            && (Draft.publicationContent revised.article).slug == (Draft.publicationContent ready).slug
            && excerptText (Draft.publicationContent revised.article).excerpt == "Edited"
            && (Draft.draftTimeline revised.article).createdAt == timestamp 0
        )
    forM_
        [ Article.Unvalidated initial
        , Article.Ready ready
        , Article.Published published
        , Article.Private private
        ]
        $ \state -> do
            writeIORef saved []
            outcome <- Tx.prepareToPublish (dependencies state (Right ())) generated
            check "generated result cannot overwrite other states" $ case outcome of
                Left (OperationNotAllowed _) -> True
                _ -> False
            check "wrong generation state not saved" . null =<< readIORef saved
    forM_
        [ Article.Unvalidated initial
        , Article.Proofreaded proof
        , Article.Published published
        , Article.Private private
        ]
        $ \state -> do
            outcome <- Tx.prepareToPublish (dependencies state (Right ())) revision
            check "manual revision requires ready" $ case outcome of
                Left (OperationNotAllowed _) -> True
                _ -> False
            check "wrong manual state not saved" . null =<< readIORef saved
    forM_ ["", "  ", Text.replicate 201 "a"] $ \invalid -> do
        writeIORef loads []
        request <- command (Prepare.ApplyGeneratedExcerpt value invalid)
        outcome <- Tx.prepareToPublish (dependencies (Article.Proofreaded proof) (Right ())) request
        case newExcerpt invalid of
            Left err -> expectError "excerpt validation" err outcome
            Right _ -> fail "bad fixture"
        check "invalid excerpt rejected before loading" . null =<< readIORef loads
    let loadFailure = createServiceUnavailable "Article" "unavailable"
        noUse = const (fail "wrong loader selected")
    failedLoad <-
        Tx.prepareToPublish
            (Tx.prepareToPublishDependencies context (const (pure (Left loadFailure))) noUse)
            generated
    expectError "load failure propagated" loadFailure failedLoad
    missing <-
        Tx.prepareToPublish
            (Tx.prepareToPublishDependencies context noUse (const (pure (Right Nothing))))
            revision
    expectError "missing article" (createAggregateNotFound "Article" (articleIdentifierText value)) missing
    other <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    wrongRequest <- command (Prepare.ApplyGeneratedExcerpt other "Generated")
    wrong <- Tx.prepareToPublish (dependencies (Article.Proofreaded proof) (Right ())) wrongRequest
    expectError
        "wrong identity"
        ( createUnexpectedError
            "Article"
            "loaded identity does not match request"
        )
        wrong
    futureProof <- right (Draft.proofread (timestamp 20) available initial)
    stale <- Tx.prepareToPublish (dependencies (Article.Proofreaded futureProof) (Right ())) generated
    check "stale timestamp rejected" $ case stale of
        Left (InvariantViolation _) -> True
        _ -> False
    check "stale timestamp not saved" . null =<< readIORef saved
    forM_
        [ createOperationNotAllowed "Article" "stale revision"
        , createServiceUnavailable "Outbox" "commit failed"
        ]
        $ \err -> do
            forM_ [(Article.Proofreaded proof, generated), (Article.Ready ready, revision)] $ \(state, request) -> do
                writeIORef saved []
                outcome <- Tx.prepareToPublish (dependencies state (Left err)) request
                expectError "conditional save error" err outcome
                check "single commit attempt" . (== 1) . length =<< readIORef saved
