{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.DomainOutbox (
    appendDraftStarted,
    appendDraftAmended,
    appendPublished,
    appendTakenDown,
    appendDiscarded,
    appendImageEventWith,
    appendReferenceEventWith,
) where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy qualified as Lazy
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import "article" Domain.Article (
    ArticleIdentifier,
    articleIdentifierText,
    imageReferenceText,
 )
import "article" Domain.Article.Event (ImageReferences (..))
import Infrastructure.Article.DurableObject.AlarmSchedule (scheduleOutboxAlarmSoon)
import "article" UseCase.Result (ArticleEventsFor)
import "article" UseCase.Result qualified as Result
import Infrastructure.Article.DurableObject.Repository (OutboxRecord (..), appendOutbox)
import Infrastructure.Article.DurableObject.Transaction (ArticleTransactionContext (..))
import "shared" Shared.Domain.Common.Transaction (Transaction)
import "shared" Shared.Domain.Error (DomainError, createOperationNotAllowed)
import "shared" Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import "shared" Shared.Infrastructure.Transaction (transactionAction)
import "shared" Shared.UseCase.Command (Command (..))
import "shared" Shared.UseCase.Context (actorText, causationText, correlationIdentifierText)
import "shared" Shared.UseCase.Event (EventIdentifier, eventIdentifierText)
import "shared" Shared.UseCase.Outbox (Append)

type NewEventIdentifier = IO (Either DomainError EventIdentifier)

appendDraftStarted ::
    NewEventIdentifier ->
    Append (ArticleEventsFor 'Result.JotDown) (Transaction ArticleTransactionContext IO)
appendDraftStarted = scheduled . appendImageEvents "ArticleDraftStarted" appendToStorage

appendDraftAmended ::
    NewEventIdentifier ->
    Append (ArticleEventsFor 'Result.AmendDraft) (Transaction ArticleTransactionContext IO)
appendDraftAmended = scheduled . appendImageEvents "ArticleDraftAmended" appendToStorage

appendPublished ::
    NewEventIdentifier ->
    Append (ArticleEventsFor 'Result.Publish) (Transaction ArticleTransactionContext IO)
appendPublished = scheduled . appendReferenceEvents "ArticlePublished" appendToStorage

appendTakenDown ::
    NewEventIdentifier ->
    Append (ArticleEventsFor 'Result.TakeDown) (Transaction ArticleTransactionContext IO)
appendTakenDown = scheduled . appendReferenceEvents "ArticleTakenDown" appendToStorage

appendDiscarded ::
    NewEventIdentifier ->
    Append (ArticleEventsFor 'Result.DiscardArticle) (Transaction ArticleTransactionContext IO)
appendDiscarded = scheduled . appendReferenceEvents "ArticleDiscarded" appendToStorage

scheduled ::
    (Command () -> events -> Transaction ArticleTransactionContext IO ()) ->
    Command () -> events -> Transaction ArticleTransactionContext IO ()
scheduled append command events = do
    append command events
    transactionAction $ \context -> do
        scheduleOutboxAlarmSoon context.storage
        pure (Right ())

appendToStorage ::
    ArticleTransactionContext -> OutboxRecord -> IO (Either DomainError ())
appendToStorage context = appendOutbox context.storage

appendImageEvents ::
    Text ->
    (context -> OutboxRecord -> IO (Either DomainError ())) ->
    NewEventIdentifier ->
    Command () ->
    Events '[DomainEvent kind ImageReferences] ->
    Transaction context IO ()
appendImageEvents kind append newIdentifier command events = case events of
    Events [Here (DomainEvent references)] ->
        appendImageEventWith append kind newIdentifier command references
    _ -> invalidEvents kind

appendReferenceEvents ::
    Text ->
    (context -> OutboxRecord -> IO (Either DomainError ())) ->
    NewEventIdentifier ->
    Command () ->
    Events '[DomainEvent kind ArticleIdentifier] ->
    Transaction context IO ()
appendReferenceEvents kind append newIdentifier command events = case events of
    Events [Here (DomainEvent article)] ->
        appendReferenceEventWith append kind newIdentifier command article
    _ -> invalidEvents kind

appendImageEventWith ::
    (context -> OutboxRecord -> IO (Either DomainError ())) ->
    Text -> NewEventIdentifier -> Command () -> ImageReferences -> Transaction context IO ()
appendImageEventWith append kind newIdentifier command references =
    appendEventWith append kind newIdentifier command references.article
        (object
            [ "article" .= articleIdentifierText references.article
            , "images" .= map imageReferenceText (Set.toAscList references.images)
            ])

appendReferenceEventWith ::
    (context -> OutboxRecord -> IO (Either DomainError ())) ->
    Text -> NewEventIdentifier -> Command () -> ArticleIdentifier -> Transaction context IO ()
appendReferenceEventWith append kind newIdentifier command article =
    appendEventWith append kind newIdentifier command article
        (object ["article" .= articleIdentifierText article])

appendEventWith ::
    (context -> OutboxRecord -> IO (Either DomainError ())) ->
    Text -> NewEventIdentifier -> Command () -> ArticleIdentifier -> Value -> Transaction context IO ()
appendEventWith append kind newIdentifier command article event =
    transactionAction $ \context -> do
        created <- newIdentifier
        case created of
            Left err -> pure (Left err)
            Right identifier ->
                append context
                    OutboxRecord
                        { identifier = eventIdentifierText identifier
                        , article
                        , eventKind = kind
                        , payload = decodeUtf8 $ Lazy.toStrict $ encode $ object
                            [ "identifier" .= eventIdentifierText identifier
                            , "occurredAt" .= command.timestamp
                            , "actor" .= actorText command.actor
                            , "correlation" .= correlationIdentifierText command.correlation
                            , "causation" .= fmap causationText command.causation
                            , "event" .= event
                            ]
                        , expectedRevision = Nothing
                        }

invalidEvents :: Text -> Transaction context IO a
invalidEvents kind = transactionAction $ \_ -> pure
    (Left (createOperationNotAllowed "ArticleOutbox" ("expected one " <> kind <> " event")))
