module UseCase.Transaction.WriteSpec (run) where

import Control.Monad (forM_)
import Data.IORef
import Data.Set qualified as Set
import Domain.Article (Article (..))
import Domain.Article.Draft qualified as Draft
import Domain.Article.Private qualified as Private
import Domain.Article.Published qualified as Published
import Shared.Domain.Error (DomainError (..))
import Shared.Domain.Excerpt (newExcerpt)
import Shared.Infrastructure.Versioning (initialVersion)
import TestSupport
import UseCase.AmendDraft qualified as Amend
import UseCase.DiscardArticle qualified as Discard
import UseCase.JotDown qualified as Jot
import UseCase.PrepareToPublish qualified as Prepare
import UseCase.Proofread qualified as Proof
import UseCase.Publish qualified as Publish
import UseCase.ResumePublication qualified as Resume
import UseCase.TakeDown qualified as Take
import UseCase.TestSupport (amendmentPayload, command, expectError)
import UseCase.Transaction.Fixture

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
    jot <- command input
    amend <- command (amendmentPayload value input)
    proofread <- command (Proof.ProofreadPayload value)
    prepare <- command (Prepare.ApplyGeneratedExcerpt value "Generated")
    revision <- command (Prepare.ReviseExcerpt value "Revised")
    publish <- command (Publish.PublishPayload value)
    takeDown <- command (Take.TakeDownPayload value)
    resume <- command (Resume.ResumePublicationPayload value)
    discard <- command (Discard.DiscardArticlePayload value)
    let cases =
            [
                ( "jot"
                , Nothing
                , "insert"
                , "started"
                , \f ->
                    (() <$)
                        <$> Jot.jotDown
                            (Jot.Dependencies f.manager (pure (Right value)) extractImages persistArticle (appendEvents "started"))
                            jot
                )
            ,
                ( "amend"
                , Just (Unvalidated initial)
                , "persist"
                , "amended"
                , \f ->
                    (() <$)
                        <$> Amend.amendDraft
                            (Amend.Dependencies f.manager findArticle persistArticle (appendEvents "amended") extractImages)
                            amend
                )
            ,
                ( "proofread"
                , Just (Unvalidated initial)
                , "persist"
                , "proofreaded"
                , \f ->
                    (() <$)
                        <$> Proof.proofread
                            ( Proof.Dependencies
                                f.manager
                                findArticle
                                persistArticle
                                (appendEvents "proofreaded")
                                (const (pure (Right (Set.singleton reference))))
                            )
                            proofread
                )
            ,
                ( "prepare"
                , Just (Proofreaded proof)
                , "persist"
                , "ready"
                , \f ->
                    (() <$)
                        <$> Prepare.prepareToPublish
                            (Prepare.Dependencies f.manager (findExpected initialVersion) persistArticle (appendEvents "ready"))
                            prepare
                )
            ,
                ( "publish"
                , Just (Ready ready)
                , "persist"
                , "published"
                , \f ->
                    (() <$) <$> Publish.publish (Publish.Dependencies f.manager findArticle persistArticle (appendEvents "published")) publish
                )
            ,
                ( "take down"
                , Just (Published published)
                , "persist"
                , "taken-down"
                , \f ->
                    (() <$) <$> Take.takeDown (Take.Dependencies f.manager findArticle persistArticle (appendEvents "taken-down")) takeDown
                )
            ,
                ( "discard"
                , Just (Private private)
                , "terminate"
                , "discarded"
                , \f ->
                    (() <$)
                        <$> Discard.discardArticle
                            (Discard.Dependencies f.manager findArticle terminateArticle (appendEvents "discarded"))
                            discard
                )
            ]
    forM_ cases $ \(label, source, write, event, invoke) -> do
        fixture <- newFixture source NoFailure
        result <- invoke fixture
        check (label <> " success") (result == Right ())
        stored <- readIORef fixture.stored
        check (label <> " one event") (stored.outbox == [event])
        actions <- readIORef fixture.trace
        let prefix = case label of
                "jot" -> ["begin"]
                "proofread" -> ["begin", "find", "commit", "begin", "find"]
                "prepare" -> ["begin", "generation", "find"]
                _ -> ["begin", "find"]
        check (label <> " ordering") (actions == prefix <> [write, "outbox:" <> event, "commit"])
        forM_ [FailWrite, FailOutbox, FailCommit] $ \failure -> do
            failed <- newFixture source failure
            outcome <- invoke failed
            expectError (label <> " error preserved") storageFailure outcome
            check (label <> " rollback both stores") . (== Store source []) =<< readIORef failed.stored
            failedActions <- readIORef failed.trace
            check (label <> " never commits writes on failure") (last failedActions == "rollback")
            if failure == FailWrite
                then check (label <> " stops before outbox") (not (("outbox:" <> event) `elem` failedActions))
                else pure ()
    forM_
        [ (Just (Private private), \f -> (() <$) <$> Resume.resumePublication (Resume.Dependencies f.manager findArticle persistArticle) resume)
        ,
            ( Just (Ready ready)
            , \f ->
                (() <$)
                    <$> Prepare.prepareToPublish
                        (Prepare.Dependencies f.manager findArticle persistArticle (appendEvents "ready"))
                        revision
            )
        ]
        $ \(source, invoke) -> do
            fixture <- newFixture source NoFailure
            outcome <- invoke fixture
            check "eventless success" (outcome == Right ())
            check "eventless transaction" . (== ["begin", "find", "persist", "commit"]) =<< readIORef fixture.trace
            forM_ [FailWrite, FailCommit] $ \failure -> do
                failed <- newFixture source failure
                result <- invoke failed
                expectError "eventless failure" storageFailure result
                check "eventless rollback" . (== Store source []) =<< readIORef failed.stored
    unknown <- newFixture (Just (Ready ready)) UnknownCommit
    outcome <- Publish.publish (Publish.Dependencies unknown.manager findArticle persistArticle (appendEvents "published")) publish
    check "unknown not success" $ case outcome of
        Left (TransactionOutcomeUnknown _) -> True
        _ -> False
    check "unknown not retried" . (== ["begin", "find", "persist", "outbox:published", "unknown"]) =<< readIORef unknown.trace
    wrongState <- newFixture (Just (Unvalidated initial)) NoFailure
    rejected <- Publish.publish (Publish.Dependencies wrongState.manager findArticle persistArticle (appendEvents "published")) publish
    check "domain failure" $ case rejected of
        Left (OperationNotAllowed _) -> True
        _ -> False
    check "domain failure rolls back" . (== ["begin", "find", "rollback"]) =<< readIORef wrongState.trace
