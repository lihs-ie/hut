{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.ProofreadOutbox (
    appendProofreadOutbox,
    appendProofreadOutboxWith,
    appendProofreadOutboxScheduledWith,
    recordRegenerationRequest,
    recordGenerationRequestScheduledWith,
) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLLimits (..),
    sqlExec,
 )
import Cloudflare.Workers.Binding.DurableObject (doStorageSetAlarm)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (IORef, readIORef)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Text.Encoding (decodeUtf8)
import "article" Domain.Article.Common (ArticleIdentifier)
import Infrastructure.Article.DurableObject.GenerationJob (
    GenerationRequestDecision (..),
    requestGenerationWith,
 )
import Infrastructure.Article.DurableObject.Repository (
    ExecuteSQL,
    OutboxRecord (..),
    appendOutboxWith,
 )
import Infrastructure.Article.DurableObject.Transaction (ArticleTransactionContext (..))
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerationRequested (..),
    ExcerptGenerationRequestedMessage (..),
    GenerationRequestIdentifier,
 )
import "shared" Shared.Domain.Common.Transaction (Transaction)
import "shared" Shared.Domain.Error (DomainError, createOperationNotAllowed)
import "shared" Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import "shared" Shared.Infrastructure.Transaction (transactionAction)
import "shared" Shared.Infrastructure.Versioning (
    PersistenceMode (..),
    VersionContext,
    persistenceMode,
 )
import "shared" Shared.UseCase.Command (Command (..))
import "shared" Shared.UseCase.Event (
    EventIdentifier,
    eventIdentifierText,
    newEventEnvelope,
 )
import "shared" Shared.UseCase.Outbox (Append)
import "article" UseCase.Result (ArticleEventsFor, ArticleUseCase (Proofread))

-- The caller must run this after persistence in the same DO transaction.
appendProofreadOutbox ::
    IO (Either DomainError GenerationRequestIdentifier) ->
    IO (Either DomainError EventIdentifier) ->
    Append (ArticleEventsFor 'Proofread) (Transaction ArticleTransactionContext IO)
appendProofreadOutbox =
    appendProofreadOutboxScheduledWith executeArticleSQL (.versions) scheduleDelivery

recordRegenerationRequest ::
    IO (Either DomainError GenerationRequestIdentifier) ->
    IO (Either DomainError EventIdentifier) ->
    Command () ->
    ArticleIdentifier ->
    Transaction ArticleTransactionContext IO GenerationRequestIdentifier
recordRegenerationRequest =
    recordGenerationRequestScheduledWith executeArticleSQL (.versions) scheduleDelivery

executeArticleSQL :: ArticleTransactionContext -> ExecuteSQL
executeArticleSQL context =
    sqlExec context.storage
        SQLLimits
            { maximumRows = 2
            , maximumBytes = 16777216
            , maximumStatements = 1
            }

scheduleDelivery :: ArticleTransactionContext -> IO (Either DomainError ())
scheduleDelivery context = do
    now <- getCurrentTime
    let milliseconds = floor (utcTimeToPOSIXSeconds now * 1000)
    doStorageSetAlarm context.storage (milliseconds + 1000)
    pure (Right ())

appendProofreadOutboxScheduledWith ::
    (context -> ExecuteSQL) ->
    (context -> IORef (VersionContext ArticleIdentifier)) ->
    (context -> IO (Either DomainError ())) ->
    IO (Either DomainError GenerationRequestIdentifier) ->
    IO (Either DomainError EventIdentifier) ->
    Append (ArticleEventsFor 'Proofread) (Transaction context IO)
appendProofreadOutboxScheduledWith execute versions schedule newRequest newEvent command events = do
    appendProofreadOutboxWith execute versions newRequest newEvent command events
    transactionAction schedule

recordGenerationRequestScheduledWith ::
    (context -> ExecuteSQL) ->
    (context -> IORef (VersionContext ArticleIdentifier)) ->
    (context -> IO (Either DomainError ())) ->
    IO (Either DomainError GenerationRequestIdentifier) ->
    IO (Either DomainError EventIdentifier) ->
    Command () ->
    ArticleIdentifier ->
    Transaction context IO GenerationRequestIdentifier
recordGenerationRequestScheduledWith execute versions schedule newRequest newEvent command article = do
    identifier <- recordGenerationRequestWith execute versions newRequest newEvent command article
    transactionAction schedule
    pure identifier

-- Inject both accessors so tests can use the same transaction-local SQL and
-- version context without constructing a DurableObjectStorage binding.
appendProofreadOutboxWith ::
    (context -> ExecuteSQL) ->
    (context -> IORef (VersionContext ArticleIdentifier)) ->
    IO (Either DomainError GenerationRequestIdentifier) ->
    IO (Either DomainError EventIdentifier) ->
    Append (ArticleEventsFor 'Proofread) (Transaction context IO)
appendProofreadOutboxWith execute versions newRequest newEvent command events =
    case events of
        Events [Here (DomainEvent article)] ->
            () <$ recordGenerationRequestWith execute versions newRequest newEvent command article
        _ -> transactionAction $ \_ -> pure (Left (createOperationNotAllowed
            "ProofreadOutbox" "expected exactly one ArticleProofreaded event"))

recordGenerationRequestWith ::
    (context -> ExecuteSQL) ->
    (context -> IORef (VersionContext ArticleIdentifier)) ->
    IO (Either DomainError GenerationRequestIdentifier) ->
    IO (Either DomainError EventIdentifier) ->
    Command () ->
    ArticleIdentifier ->
    Transaction context IO GenerationRequestIdentifier
recordGenerationRequestWith execute versions newRequest newEvent command article =
    transactionAction $ \context -> do
        observed <- readIORef (versions context)
        case persistenceMode article observed of
            Left err -> pure (Left err)
            Right Insert ->
                pure (Left (createOperationNotAllowed
                    "ExcerptGeneration"
                    "article must be persisted before requesting generation"))
            Right (Update revision) -> do
                offered <- newRequest
                case offered of
                    Left err -> pure (Left err)
                    Right requestIdentifier -> do
                        decision <- requestGenerationWith
                            (execute context) article revision requestIdentifier
                        case decision of
                            Left err -> pure (Left err)
                            Right (GenerationReused request) ->
                                pure (Right request.identifier)
                            Right (GenerationCreated request) -> do
                                generated <- newEvent
                                case generated of
                                    Left err -> pure (Left err)
                                    Right identifier -> do
                                        let envelope =
                                                newEventEnvelope
                                                    identifier
                                                    command.timestamp
                                                    command.actor
                                                    command.correlation
                                                    command.causation
                                                    request
                                            record =
                                                OutboxRecord
                                                    { identifier = eventIdentifierText identifier
                                                    , article = article
                                                    , eventKind = "ExcerptGenerationRequested"
                                                    , payload = decodeUtf8 (Lazy.toStrict (encode
                                                        (ExcerptGenerationRequestedMessage envelope)))
                                                    , expectedRevision = Just revision
                                                    }
                                        appended <- appendOutboxWith (execute context) record
                                        pure (request.identifier <$ appended)
