{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.ProofreadOutboxSpec (run) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLError (..),
    SQLResult (..),
    SQLStatement (..),
    SQLValue (..),
 )
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Aeson (eitherDecodeStrict')
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import "article" Domain.Article.Common (ArticleIdentifier, newArticleIdentifier)
import Infrastructure.Article.DurableObject.ProofreadOutbox (
    appendProofreadOutboxScheduledWith,
    appendProofreadOutboxWith,
    recordGenerationRequestScheduledWith,
 )
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerationRequested (..),
    ExcerptGenerationRequestedMessage (..),
    GenerationRequestIdentifier,
    newGenerationRequestIdentifier,
 )
import "shared" Shared.Domain.Common.Primitive (newPositiveInteger)
import "shared" Shared.Domain.Common.Transaction (runTransaction)
import "shared" Shared.Domain.Error (DomainError (..), createServiceUnavailable)
import "shared" Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import "shared" Shared.Infrastructure.Transaction (
    TransactionDriver (..),
    TransactionOutcome (..),
    newTransactionManager,
 )
import "shared" Shared.Infrastructure.Versioning (
    Version,
    VersionContext,
    emptyVersionContext,
    newVersion,
    recordPersisted,
 )
import "shared" Shared.UseCase.Command (
    Command (..),
    newActor,
    newCausation,
    newCorrelationIdentifier,
 )
import "shared" Shared.UseCase.Event (
    EventEnvelope (..),
    EventIdentifier,
    newEventIdentifier,
 )
import "article" UseCase.Result (ArticleEventsFor, ArticleUseCase (Proofread))

data TestContext = TestContext
    { execute :: ExecuteSQL
    , versions :: IORef (VersionContext ArticleIdentifier)
    }

check :: String -> Bool -> IO ()
check label condition = unless condition (fail label)

right :: (Show errorType) => Either errorType value -> IO value
right = either (fail . show) pure

article :: IO ArticleIdentifier
article = right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")

requestIdentifier :: IO GenerationRequestIdentifier
requestIdentifier = right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")

eventIdentifier :: IO EventIdentifier
eventIdentifier = right (newEventIdentifier "generation-event")

revision :: Integer -> IO Version
revision value = newVersion <$> right (newPositiveInteger value)

fixedTime :: UTCTime
fixedTime = read "2026-01-01 00:00:00 UTC"

command :: IO (Command ())
command =
    Command () fixedTime
        <$> right (newActor "editor")
        <*> right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
        <*> (Just <$> right (newCausation "proofread-command"))

proofreaded :: ArticleIdentifier -> Events (ArticleEventsFor 'Proofread)
proofreaded target = Events [Here (DomainEvent target)]

emptyResult :: SQLResult
emptyResult = SQLResult [] [] 0 0

oneRow :: [SQLValue] -> SQLResult
oneRow row = SQLResult [] [row] 0 1

newScript :: [Either SQLError SQLResult] -> IO (ExecuteSQL, IO [SQLStatement])
newScript responses = do
    remaining <- newIORef responses
    statements <- newIORef []
    let executeSQL statement = do
            modifyIORef' statements (<> [statement])
            next <- atomicModifyIORef' remaining $ \values -> case values of
                [] -> ([], Nothing)
                value : rest -> (rest, Just value)
            maybe (fail "unexpected SQL statement") (either throwIO pure) next
    pure (executeSQL, readIORef statements)

newContext :: ExecuteSQL -> ArticleIdentifier -> Maybe Version -> IO TestContext
newContext execute target persisted = do
    let initial = emptyVersionContext "Article" (const "article")
        observed = maybe initial (\value -> recordPersisted target value initial) persisted
    versions <- newIORef observed
    pure TestContext{execute, versions}

runAppend ::
    TestContext ->
    IO GenerationRequestIdentifier ->
    IO EventIdentifier ->
    Command () ->
    Events (ArticleEventsFor 'Proofread) ->
    IO (Either DomainError ())
runAppend context newRequest newEvent source events =
    runTransaction (newTransactionManager driver) $
        appendProofreadOutboxWith (.execute) (.versions)
            (Right <$> newRequest) (Right <$> newEvent) source events
  where
    driver = TransactionDriver $ \action -> do
        result <- action context
        pure (either RolledBack Committed result)

run :: IO ()
run = do
    appendsOneEnvelopedRequest
    reusesWithoutAppending
    rejectsUnpersistedArticle
    rejectsUnboundedEvents
    stopsOnStaleRevision
    stopsOnJobFailure
    reportsOutboxFailure
    schedulesAfterAppend
    recordsRegenerationRequest
    reusesRegenerationRequest
    rejectsRegenerationFailures
    rejectsInvalidRegenerationTargets

appendsOneEnvelopedRequest :: IO ()
appendsOneEnvelopedRequest = do
    target <- article
    offered <- requestIdentifier
    newEvent <- eventIdentifier
    current <- revision 3
    source <- command
    (executeSQL, issued) <- newScript
        [ Right (oneRow [SQLNumber 3])
        , Right emptyResult
        , Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"])
        , Right (oneRow [SQLText "generation-event"])
        ]
    context <- newContext executeSQL target (Just current)
    requestCalls <- newIORef (0 :: Int)
    eventCalls <- newIORef (0 :: Int)
    let newRequest = modifyIORef' requestCalls (+ 1) >> pure offered
        newIdentifier = modifyIORef' eventCalls (+ 1) >> pure newEvent
    outcome <- runAppend context newRequest newIdentifier source (proofreaded target)
    check "created request appended" (outcome == Right ())
    check "request identifier generated once" =<< (== 1) <$> readIORef requestCalls
    check "event identifier generated once" =<< (== 1) <$> readIORef eventCalls
    statements <- issued
    case statements of
        [revisionQuery, activeQuery, jobInsert, outboxInsert] -> do
            check "read stored revision first"
                ("SELECT revision FROM article_aggregates" `Text.isInfixOf` revisionQuery.sql)
            check "check active job before inserting"
                ("SELECT request_identifier, revision" `Text.isInfixOf` activeQuery.sql)
            check "insert generation job before outbox"
                ("INSERT INTO article_generation_jobs" `Text.isInfixOf` jobInsert.sql)
            check "outbox inserted once"
                ("INSERT INTO article_outbox" `Text.isInfixOf` outboxInsert.sql)
            case outboxInsert.parameters of
                [SQLText identifier, SQLText articleText, SQLText kind,
                    SQLText payload, SQLNumber expected] -> do
                    check "outbox metadata"
                        ( identifier == "generation-event"
                            && articleText == "01ARZ3NDEKTSV4RRFFQ69G5FAV"
                            && kind == "ExcerptGenerationRequested"
                            && expected == 3
                        )
                    message <- right (eitherDecodeStrict' (encodeUtf8 payload))
                    check "envelope and request retain command metadata"
                        ( message == ExcerptGenerationRequestedMessage
                            (EventEnvelope newEvent fixedTime source.actor
                                source.correlation source.causation
                                (ExcerptGenerationRequested offered target current))
                        )
                _ -> fail "invalid outbox insert parameters"
        _ -> fail "expected revision, active job, job insert and outbox insert"

reusesWithoutAppending :: IO ()
reusesWithoutAppending = do
    target <- article
    current <- revision 3
    source <- command
    (executeSQL, issued) <- newScript
        [ Right (oneRow [SQLNumber 3])
        , Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAY", SQLNumber 3])
        ]
    context <- newContext executeSQL target (Just current)
    outcome <- runAppend context requestIdentifier
        (fail "reused request must not generate an event") source (proofreaded target)
    check "reused request succeeds" (outcome == Right ())
    check "reuse has no insert" =<< (== 2) . length <$> issued

rejectsUnpersistedArticle :: IO ()
rejectsUnpersistedArticle = do
    target <- article
    source <- command
    (executeSQL, issued) <- newScript []
    context <- newContext executeSQL target Nothing
    outcome <- runAppend context
        (fail "unpersisted article must not generate a request")
        (fail "unpersisted article must not generate an event")
        source (proofreaded target)
    check "missing persisted revision rejected" (isOperationNotAllowed outcome)
    check "missing revision does not query SQL" =<< null <$> issued

rejectsUnboundedEvents :: IO ()
rejectsUnboundedEvents = do
    target <- article
    current <- revision 3
    source <- command
    (executeSQL, issued) <- newScript []
    context <- newContext executeSQL target (Just current)
    let reject events = runAppend context
            (fail "invalid events must not generate a request")
            (fail "invalid events must not generate an event") source events
    empty <- reject (Events [])
    duplicate <- reject (Events [Here (DomainEvent target), Here (DomainEvent target)])
    check "zero and duplicate proofread events rejected"
        (isOperationNotAllowed empty && isOperationNotAllowed duplicate)
    check "invalid events do not query SQL" =<< null <$> issued

stopsOnStaleRevision :: IO ()
stopsOnStaleRevision = do
    target <- article
    current <- revision 3
    source <- command
    (executeSQL, issued) <- newScript [Right (oneRow [SQLNumber 4])]
    context <- newContext executeSQL target (Just current)
    outcome <- runAppend context requestIdentifier
        (fail "stale revision must not generate an event") source (proofreaded target)
    check "stale persisted revision rejected" (isChanged outcome)
    check "stale revision stops before job and outbox writes" =<< (== 1) . length <$> issued

stopsOnJobFailure :: IO ()
stopsOnJobFailure = do
    target <- article
    current <- revision 3
    source <- command
    (executeSQL, issued) <- newScript [Left (SQLError "job unavailable")]
    context <- newContext executeSQL target (Just current)
    outcome <- runAppend context requestIdentifier
        (fail "failed job must not generate an event") source (proofreaded target)
    check "job SQL failure is returned" (isUnavailable outcome)
    check "job failure prevents outbox write" =<< (== 1) . length <$> issued

reportsOutboxFailure :: IO ()
reportsOutboxFailure = do
    target <- article
    current <- revision 3
    source <- command
    (executeSQL, issued) <- newScript
        [ Right (oneRow [SQLNumber 3])
        , Right emptyResult
        , Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"])
        , Right emptyResult
        ]
    context <- newContext executeSQL target (Just current)
    outcome <- runAppend context requestIdentifier eventIdentifier source (proofreaded target)
    check "outbox conflict aborts transaction action" (isOperationNotAllowed outcome)
    check "outbox failure follows job creation" =<< (== 4) . length <$> issued

schedulesAfterAppend :: IO ()
schedulesAfterAppend = do
    target <- article
    current <- revision 3
    source <- command
    (executeSQL, _) <- newScript
        [ Right (oneRow [SQLNumber 3])
        , Right emptyResult
        , Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"])
        , Right (oneRow [SQLText "generation-event"])
        ]
    context <- newContext executeSQL target (Just current)
    called <- newIORef False
    let driver = TransactionDriver $ \action -> do
            result <- action context
            pure (either RolledBack Committed result)
        append schedule = runTransaction (newTransactionManager driver) $
            appendProofreadOutboxScheduledWith
                (.execute)
                (.versions)
                schedule
                (Right <$> requestIdentifier)
                (Right <$> eventIdentifier)
                source
                (proofreaded target)
    outcome <- append (\_ -> writeIORef called True >> pure (Right ()))
    check "successful append schedules delivery" (outcome == Right ())
    check "delivery alarm was set" =<< readIORef called

    (failingSQL, _) <- newScript
        [ Right (oneRow [SQLNumber 3])
        , Right emptyResult
        , Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"])
        , Right (oneRow [SQLText "generation-event"])
        ]
    failingContext <- newContext failingSQL target (Just current)
    let failingDriver = TransactionDriver $ \action -> do
            result <- action failingContext
            pure (either RolledBack Committed result)
        unavailable = createServiceUnavailable "ArticleAlarm" "unavailable"
    failed <- runTransaction (newTransactionManager failingDriver) $
        appendProofreadOutboxScheduledWith
            (.execute)
            (.versions)
            (\_ -> pure (Left unavailable))
            (Right <$> requestIdentifier)
            (Right <$> eventIdentifier)
            source
            (proofreaded target)
    check "alarm failure aborts append transaction" (failed == Left unavailable)

runRecord ::
    TestContext ->
    (TestContext -> IO (Either DomainError ())) ->
    IO (Either DomainError GenerationRequestIdentifier) ->
    IO (Either DomainError EventIdentifier) ->
    Command () ->
    ArticleIdentifier ->
    IO (Either DomainError GenerationRequestIdentifier)
runRecord context schedule newRequest newEvent source target =
    runTransaction (newTransactionManager driver) $
        recordGenerationRequestScheduledWith
            (.execute)
            (.versions)
            schedule
            newRequest
            newEvent
            source
            target
  where
    driver = TransactionDriver $ \action -> do
        result <- action context
        pure (either RolledBack Committed result)

recordsRegenerationRequest :: IO ()
recordsRegenerationRequest = do
    target <- article
    offered <- requestIdentifier
    event <- eventIdentifier
    current <- revision 3
    source <- command
    (executeSQL, issued) <- newScript
        [ Right (oneRow [SQLNumber 3])
        , Right emptyResult
        , Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"])
        , Right (oneRow [SQLText "generation-event"])
        ]
    context <- newContext executeSQL target (Just current)
    scheduled <- newIORef False
    outcome <- runRecord context
        (\_ -> writeIORef scheduled True >> pure (Right ()))
        (pure (Right offered))
        (pure (Right event))
        source target
    check "new regeneration request returned" (outcome == Right offered)
    check "regeneration delivery scheduled" =<< readIORef scheduled
    statements <- issued
    check "regeneration persisted generation job and event"
        (length statements == 4)

reusesRegenerationRequest :: IO ()
reusesRegenerationRequest = do
    target <- article
    offered <- requestIdentifier
    existing <- right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAY")
    current <- revision 3
    source <- command
    (executeSQL, issued) <- newScript
        [ Right (oneRow [SQLNumber 3])
        , Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAY", SQLNumber 3])
        ]
    context <- newContext executeSQL target (Just current)
    outcome <- runRecord context
        (const (pure (Right ())))
        (pure (Right offered))
        (fail "reuse must not generate event")
        source target
    check "active request reused" (outcome == Right existing)
    check "reuse leaves outbox untouched" =<< (== 2) . length <$> issued

rejectsRegenerationFailures :: IO ()
rejectsRegenerationFailures = do
    target <- article
    current <- revision 3
    source <- command
    let unavailable = createServiceUnavailable "Generation" "failed"
        schedule = const (pure (Right ()))

    (requestSQL, requestIssued) <- newScript []
    requestContext <- newContext requestSQL target (Just current)
    requestFailure <- runRecord requestContext schedule
        (pure (Left unavailable))
        (fail "request failure must not generate event") source target
    check "identifier failure is propagated" (requestFailure == Left unavailable)
    check "identifier failure skips SQL" =<< null <$> requestIssued

    (eventSQL, eventIssued) <- newScript
        [Right (oneRow [SQLNumber 3]), Right emptyResult,
         Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"])]
    eventContext <- newContext eventSQL target (Just current)
    eventFailure <- runRecord eventContext schedule
        (Right <$> requestIdentifier)
        (pure (Left unavailable)) source target
    check "event identifier failure is propagated" (eventFailure == Left unavailable)
    check "event failure skips outbox" =<< (== 3) . length <$> eventIssued

    (alarmSQL, alarmIssued) <- newScript
        [Right (oneRow [SQLNumber 3]), Right emptyResult,
         Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]),
         Right (oneRow [SQLText "generation-event"])]
    alarmContext <- newContext alarmSQL target (Just current)
    alarmFailure <- runRecord alarmContext
        (const (pure (Left unavailable)))
        (Right <$> requestIdentifier)
        (Right <$> eventIdentifier) source target
    check "alarm failure aborts regeneration" (alarmFailure == Left unavailable)
    check "alarm follows outbox append" =<< (== 4) . length <$> alarmIssued

rejectsInvalidRegenerationTargets :: IO ()
rejectsInvalidRegenerationTargets = do
    target <- article
    current <- revision 3
    source <- command
    let schedule = const (pure (Right ()))
        offered = Right <$> requestIdentifier
        event = Right <$> eventIdentifier

    (missingSQL, missingIssued) <- newScript []
    missingContext <- newContext missingSQL target Nothing
    missing <- runRecord missingContext schedule offered event source target
    check "unpersisted regeneration target rejected" (isOperationNotAllowed missing)
    check "unpersisted regeneration skips SQL" =<< null <$> missingIssued

    (staleSQL, staleIssued) <- newScript [Right (oneRow [SQLNumber 4])]
    staleContext <- newContext staleSQL target (Just current)
    stale <- runRecord staleContext schedule offered event source target
    check "stale regeneration target rejected" (isChanged stale)
    check "stale regeneration skips inserts" =<< (== 1) . length <$> staleIssued

    (jobSQL, jobIssued) <- newScript [Left (SQLError "job unavailable")]
    jobContext <- newContext jobSQL target (Just current)
    jobFailure <- runRecord jobContext schedule offered event source target
    check "generation job read failure propagated" (isUnavailable jobFailure)
    check "generation job failure skips inserts" =<< (== 1) . length <$> jobIssued

    (outboxSQL, outboxIssued) <- newScript
        [Right (oneRow [SQLNumber 3]), Right emptyResult,
         Right (oneRow [SQLText "01ARZ3NDEKTSV4RRFFQ69G5FAW"]),
         Right emptyResult]
    outboxContext <- newContext outboxSQL target (Just current)
    outboxFailure <- runRecord outboxContext schedule offered event source target
    check "outbox conflict aborts regeneration" (isOperationNotAllowed outboxFailure)
    check "outbox conflict follows job creation" =<< (== 4) . length <$> outboxIssued

isOperationNotAllowed :: Either DomainError value -> Bool
isOperationNotAllowed (Left (OperationNotAllowed _)) = True
isOperationNotAllowed _ = False

isChanged :: Either DomainError value -> Bool
isChanged (Left (ProcessingTargetChanged _)) = True
isChanged _ = False

isUnavailable :: Either DomainError value -> Bool
isUnavailable (Left (ServiceUnavailable _)) = True
isUnavailable _ = False
