module UseCase.Transaction.VersionSpec (run) where

import Data.IORef
import Domain.Article (Article (..))
import Domain.Article.Criteria
import Domain.Article.Draft qualified as Draft
import Shared.Domain.Common.Transaction
import Shared.Domain.Error (DomainError (..))
import Shared.Infrastructure.Versioning
import TestSupport
import UseCase.PrepareToPublish qualified as Prepare
import UseCase.TestSupport (command)
import UseCase.Transaction.Fixture

run :: IO ()
run = do
    value <- right identifier
    draft <- right start
    let article = Unvalidated draft
    new <- newFixture Nothing NoFailure
    _ <- runTransaction new.manager (persistArticle article) >>= right
    check "new article persisted" . (== Store (Just article) []) =<< readIORef new.stored
    check "initial version" . (== initialVersion) =<< readIORef new.version
    collision <- runTransaction new.manager (persistArticle article)
    check "new run never reuses prior observation" $ case collision of
        Left (OperationNotAllowed _) -> True
        _ -> False
    updates <- runTransaction new.manager $ do
        _ <- findArticle value
        persistArticle article
        persistArticle article
    _ <- right updates
    check "own writes advance the expected version" . (== nextVersion (nextVersion initialVersion)) =<< readIORef new.version

    absent <- newFixture Nothing NoFailure
    _ <- runTransaction absent.manager (findArticle value >> persistArticle article) >>= right
    check "explicit missing observation allows insert" . (== Store (Just article) []) =<< readIORef absent.stored

    deleted <- newFixture (Just article) NoFailure
    recreated <- runTransaction deleted.manager $ do
        _ <- findArticle value
        terminateArticle value
        _ <- findArticle value
        persistArticle article
    check "local deletion cannot become insert" $ case recreated of
        Left (OperationNotAllowed _) -> True
        _ -> False
    check "whole deletion rolled back" . (== Store (Just article) []) =<< readIORef deleted.stored
    _ <- runTransaction deleted.manager (findArticle value >> terminateArticle value) >>= right
    check "termination leaves no article" . (== Store Nothing []) =<< readIORef deleted.stored

    criteria <- right (newCriteria AllArticles 1 Nothing)
    searched <- newFixture (Just article) NoFailure
    _ <- runTransaction searched.manager (searchArticles criteria >> persistArticle article) >>= right
    check "search records version for update" . (== ["begin", "browse-admin", "persist", "commit"]) =<< readIORef searched.trace
    badReader <- runTransaction searched.manager (searchPublished criteria)
    check "reader rejects non-public selection" $ case badReader of
        Left (InvariantViolation _) -> True
        _ -> False

    -- The exact same injected actions bind to each manager's fresh context.
    isolated <- newFixture Nothing NoFailure
    emptyResult <- runTransaction isolated.manager (findArticle value) >>= right
    check "no state leaked between managers" (emptyResult == Nothing)

    available <- right confirmed
    proof <- right (Draft.proofread (timestamp 1) available draft)
    fixture <- newFixture (Just (Proofreaded proof)) NoFailure
    request <- command (Prepare.ApplyGeneratedExcerpt value "Generated")
    let dependencies =
            Prepare.Dependencies
                fixture.manager
                (findExpected initialVersion)
                persistArticle
                (appendEvents "ready")
    result <- Prepare.prepareToPublish dependencies request >>= right
    snapshot <- readIORef fixture.stored
    duplicate <- Prepare.prepareToPublish dependencies request
    check "late or duplicate generation is distinguishable" $ case duplicate of
        Left (ProcessingTargetChanged _) -> True
        _ -> False
    check "stale result leaves both stores unchanged" . (== snapshot) =<< readIORef fixture.stored
    check "ready result persisted" (snapshot.article == Just (Ready result.article))
    actions <- readIORef fixture.trace
    check
        "no stale append or automatic retry"
        ( actions
            == [ "begin"
               , "generation"
               , "find"
               , "persist"
               , "outbox:ready"
               , "commit"
               , "begin"
               , "generation"
               , "find"
               , "rollback"
               ]
        )
