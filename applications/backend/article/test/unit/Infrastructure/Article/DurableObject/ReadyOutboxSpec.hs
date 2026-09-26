module Infrastructure.Article.DurableObject.ReadyOutboxSpec (run) where

import Data.Aeson (Value (..), decodeStrict')
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text.Encoding (encodeUtf8)
import Domain.Article.Event (ArticleReadyToPublish)
import Infrastructure.Article.DurableObject.ReadyOutbox (appendReadyEventsWith)
import Infrastructure.Article.DurableObject.Repository (OutboxRecord (..))
import Shared.Domain.Error (createServiceUnavailable)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.Infrastructure.Transaction (runTransactionInContext)
import Shared.UseCase.Command (Command (Command), newActor, newCorrelationIdentifier)
import Shared.UseCase.Event (newEventIdentifier)
import TestSupport (check, identifier, right, timestamp)

run :: IO ()
run = do
    storesEnvelope
    emptyEventsDoNotGenerate
    propagatesFailures

storesEnvelope :: IO ()
storesEnvelope = do
    article <- right identifier
    actor <- right (newActor "system")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    event <- right (newEventIdentifier "event-1")
    records <- newIORef []
    let command = Command () (timestamp 3) actor correlation Nothing
        events = Events [Here (DomainEvent article)] :: Events '[ArticleReadyToPublish]
        append _ record = writeIORef records [record] >> pure (Right ())
        action = appendReadyEventsWith append (pure (Right event)) command events
    result <- runTransactionInContext () action
    check "ready outbox append succeeds" (result == Right ())
    recorded <- readIORef records
    case recorded of
        [record] -> do
            check "ready outbox kind" (record.eventKind == "ArticleReadyToPublish")
            check "ready outbox event identifier" (record.identifier == "event-1")
            check "ready outbox references article" (record.article == article)
            check "ready outbox does not carry generation revision" (record.expectedRevision == Nothing)
            check "ready outbox envelope JSON" $ case decodeStrict' (encodeUtf8 record.payload) of
                Just (Object _) -> True
                _ -> False
        _ -> fail "expected exactly one ready event"

emptyEventsDoNotGenerate :: IO ()
emptyEventsDoNotGenerate = do
    actor <- right (newActor "system")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    let command = Command () (timestamp 3) actor correlation Nothing
        events = Events [] :: Events '[ArticleReadyToPublish]
        action = appendReadyEventsWith
            (\_ _ -> fail "empty events must not append")
            (fail "empty events must not create an identifier")
            command
            events
    result <- runTransactionInContext () action
    check "empty ready events do not touch outbox" (result == Right ())

propagatesFailures :: IO ()
propagatesFailures = do
    article <- right identifier
    actor <- right (newActor "system")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    event <- right (newEventIdentifier "event-1")
    let command = Command () (timestamp 3) actor correlation Nothing
        events = Events [Here (DomainEvent article)] :: Events '[ArticleReadyToPublish]
        unavailable = createServiceUnavailable "ArticleOutbox" "unavailable"
    identifierFailure <- runTransactionInContext () $
        appendReadyEventsWith
            (\_ _ -> fail "identifier failure must not append")
            (pure (Left unavailable))
            command
            events
    check "identifier failure aborts" (identifierFailure == Left unavailable)
    appendFailure <- runTransactionInContext () $
        appendReadyEventsWith
            (\_ _ -> pure (Left unavailable))
            (pure (Right event))
            command
            events
    check "outbox failure aborts" (appendFailure == Left unavailable)
