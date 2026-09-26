module UseCase.RequestExcerptRegenerationSpec (run) where

import Data.IORef
import Data.List (find)
import Domain.Article (Article (..), ArticleIdentifier, FindArticle, articleIdentifier)
import Domain.Article.Draft qualified as Draft
import Shared.Domain.Common.Transaction (Transaction, TransactionManager)
import Shared.Domain.Error (DomainError (..), createServiceUnavailable)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Excerpt (newExcerpt)
import Shared.Infrastructure.Transaction
import Shared.UseCase.Command (Command (..), commandContext, newActor, newCorrelationIdentifier)
import TestSupport (check, confirmed, right, start)
import TestSupport qualified as Support
import UseCase.RequestExcerptRegeneration qualified as Regeneration

data JobStatus = Active | TerminalDLQ deriving stock (Eq, Show)

data Job = Job
    { identifier :: Int
    , article :: ArticleIdentifier
    , sourceRevision :: Int
    , status :: JobStatus
    }
    deriving stock (Eq, Show)

data Store = Store
    { current :: Maybe Article
    , revision :: Int
    , nextRequest :: Int
    , requests :: [Job]
    , outbox :: [Int]
    , contexts :: [Command ()]
    }
    deriving stock (Eq, Show)

data Context = Context
    { local :: IORef Store
    , logEntries :: IORef [String]
    , failRecord :: IORef Bool
    }

data Fixture = Fixture
    { manager :: TransactionManager Context IO
    , stored :: IORef Store
    , logEntries :: IORef [String]
    , failRecord :: IORef Bool
    }

newFixture :: Maybe Article -> IO Fixture
newFixture article = do
    stored <- newIORef (Store article 1 1 [] [] [])
    logEntries <- newIORef []
    failRecord <- newIORef False
    let manager = newTransactionManager $ TransactionDriver $ \callback -> do
            modifyIORef' logEntries (<> ["begin"])
            snapshot <- readIORef stored
            local <- newIORef snapshot
            outcome <- callback (Context local logEntries failRecord)
            case outcome of
                Left err -> do
                    modifyIORef' logEntries (<> ["rollback"])
                    pure (RolledBack err)
                Right value -> do
                    writeIORef stored =<< readIORef local
                    modifyIORef' logEntries (<> ["commit"])
                    pure (Committed value)
    pure (Fixture manager stored logEntries failRecord)

findArticle :: FindArticle (Transaction Context IO)
findArticle requested = transactionAction $ \context -> do
    modifyIORef' context.logEntries (<> ["find"])
    store <- readIORef context.local
    pure $ Right $ case store.current of
        Just article | articleIdentifier article == requested -> Just article
        _ -> Nothing

recordRequest :: Regeneration.RecordRegenerationRequest Int (Transaction Context IO)
recordRequest command requested = transactionAction $ \context -> do
    modifyIORef' context.logEntries (<> ["record"])
    store <- readIORef context.local
    let matches job =
            job.article == requested
                && job.sourceRevision == store.revision
                && job.status == Active
    case find matches store.requests of
        Just existing -> pure (Right existing.identifier)
        Nothing -> do
            let request = store.nextRequest
                job = Job request requested store.revision Active
            writeIORef
                context.local
                store
                    { nextRequest = request + 1
                    , requests = store.requests <> [job]
                    , outbox = store.outbox <> [request]
                    , contexts = store.contexts <> [command]
                    }
            failed <- readIORef context.failRecord
            pure $ if failed
                then Left (createServiceUnavailable "Outbox" "injected failure")
                else Right request

dependencies :: Fixture -> Regeneration.Dependencies Context IO Int
dependencies fixture =
    Regeneration.Dependencies
        fixture.manager
        findArticle
        recordRequest

newCommand :: ArticleIdentifier -> IO Regeneration.RequestExcerptRegenerationCommand
newCommand article = do
    actor <- right (newActor "administrator")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    pure
        ( Command
            (Regeneration.RequestExcerptRegenerationPayload article)
            (Support.timestamp 10)
            actor
            correlation
            Nothing
        )

isEmpty :: Events '[] -> Bool
isEmpty (Events values) = null values

run :: IO ()
run = do
    article <- right Support.identifier
    unvalidated <- right start
    available <- right confirmed
    proofreaded <- right (Draft.proofread (Support.timestamp 1) available unvalidated)
    command <- newCommand article

    fixture <- newFixture (Just (Proofreaded proofreaded))
    first <- Regeneration.requestExcerptRegeneration (dependencies fixture) command >>= right
    check "returns the unchanged proofread draft" (first.article == proofreaded)
    check "returns a generated infrastructure request identifier" (first.requestIdentifier == 1)
    check "returns no domain events" (isEmpty first.events)
    saved <- readIORef fixture.stored
    check "article and revision remain unchanged"
        (saved.current == Just (Proofreaded proofreaded) && saved.revision == 1)
    check "request and Outbox instruction are committed together"
        (saved.requests == [Job 1 article 1 Active] && saved.outbox == [1])
    check "command metadata reaches the transaction port" (saved.contexts == [commandContext command])
    check "one transaction records the request"
        . (== ["begin", "find", "record", "commit"])
        =<< readIORef fixture.logEntries

    duplicate <- Regeneration.requestExcerptRegeneration (dependencies fixture) command >>= right
    check "active request identifier is reused"
        (duplicate.requestIdentifier == first.requestIdentifier && isEmpty duplicate.events)
    duplicateStore <- readIORef fixture.stored
    check "duplicate does not append an Outbox instruction" (duplicateStore == saved)

    modifyIORef' fixture.stored (\store -> store{requests = [Job 1 article 1 TerminalDLQ]})
    retry <- Regeneration.requestExcerptRegeneration (dependencies fixture) command >>= right
    retryStore <- readIORef fixture.stored
    check "terminal DLQ request permits a new request"
        (retry.requestIdentifier == 2 && retryStore.outbox == [1, 2])

    modifyIORef' fixture.stored (\store -> store{revision = 2})
    changedRevision <- Regeneration.requestExcerptRegeneration (dependencies fixture) command >>= right
    check "request is scoped to infrastructure revision" (changedRevision.requestIdentifier == 3)

    missing <- newFixture Nothing
    absent <- Regeneration.requestExcerptRegeneration (dependencies missing) command
    check "missing article is rejected" $ case absent of
        Left (AggregateNotFound _) -> True
        _ -> False
    check "missing article does not reach the Outbox port"
        . (== ["begin", "find", "rollback"])
        =<< readIORef missing.logEntries

    draft <- newFixture (Just (Unvalidated unvalidated))
    wrongPhase <- Regeneration.requestExcerptRegeneration (dependencies draft) command
    check "unvalidated article is rejected" $ case wrongPhase of
        Left (OperationNotAllowed _) -> True
        _ -> False
    check "wrong phase does not reach the Outbox port"
        . (== ["begin", "find", "rollback"])
        =<< readIORef draft.logEntries

    excerpt <- right (newExcerpt "Generated excerpt")
    readyArticle <- right (Draft.prepareToPublish (Support.timestamp 2) excerpt proofreaded)
    ready <- newFixture (Just (Ready readyArticle))
    readyResult <- Regeneration.requestExcerptRegeneration (dependencies ready) command
    check "ready article is rejected" $ case readyResult of
        Left (OperationNotAllowed _) -> True
        _ -> False

    failure <- newFixture (Just (Proofreaded proofreaded))
    writeIORef failure.failRecord True
    failed <- Regeneration.requestExcerptRegeneration (dependencies failure) command
    check "Outbox failure is propagated" $ case failed of
        Left (ServiceUnavailable _) -> True
        _ -> False
    failureStore <- readIORef failure.stored
    check "failed Outbox write rolls back the request"
        (failureStore.requests == [] && failureStore.outbox == [])
    check "failed Outbox write leaves article unchanged"
        (failureStore.current == Just (Proofreaded proofreaded))
