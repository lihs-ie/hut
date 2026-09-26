{-# LANGUAGE PackageImports #-}
{-# LANGUAGE EmptyCase #-}

module Infrastructure.Article.DurableObject.ReadyOutbox (
    appendReadyEvents,
    appendReadyEventsWith,
    appendReadyEventsWithSchedule,
) where

import Data.Aeson (encode, object, (.=))
import Control.Monad (foldM)
import Data.ByteString.Lazy qualified as Lazy
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import "article" Domain.Article (articleIdentifierText)
import "article" UseCase.Result (ArticleEventsFor)
import "article" UseCase.Result qualified as Result
import Infrastructure.Article.DurableObject.AlarmSchedule (scheduleOutboxAlarmSoon)
import "shared" Shared.Domain.Common.Transaction (Transaction)
import "shared" Shared.Domain.Error (DomainError)
import "shared" Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import "shared" Shared.Infrastructure.Transaction (transactionAction)
import "shared" Shared.UseCase.Command (Command (..))
import "shared" Shared.UseCase.Context (actorText, causationText, correlationIdentifierText)
import "shared" Shared.UseCase.Event (EventIdentifier, eventIdentifierText)
import "shared" Shared.UseCase.Outbox (Append)
import Infrastructure.Article.DurableObject.Repository (OutboxRecord (..), appendOutbox)
import Infrastructure.Article.DurableObject.Transaction (ArticleTransactionContext (..))

appendReadyEvents ::
    IO (Either DomainError EventIdentifier) ->
    Append (ArticleEventsFor 'Result.PrepareToPublish)
        (Transaction ArticleTransactionContext IO)
appendReadyEvents newIdentifier command events = do
    appendReadyEventsWithSchedule
        (\context -> appendOutbox context.storage)
        newIdentifier
        schedule
        command
        events
  where
    schedule context = scheduleOutboxAlarmSoon context.storage

appendReadyEventsWithSchedule ::
    (context -> OutboxRecord -> IO (Either DomainError ())) ->
    IO (Either DomainError EventIdentifier) ->
    (context -> IO ()) ->
    Append (ArticleEventsFor 'Result.PrepareToPublish)
        (Transaction context IO)
appendReadyEventsWithSchedule append newIdentifier schedule command events = do
    appendReadyEventsWith append newIdentifier command events
    transactionAction $ \context -> schedule context >> pure (Right ())

appendReadyEventsWith ::
    (context -> OutboxRecord -> IO (Either DomainError ())) ->
    IO (Either DomainError EventIdentifier) ->
    Append (ArticleEventsFor 'Result.PrepareToPublish)
        (Transaction context IO)
appendReadyEventsWith append newIdentifier command (Events events) =
    transactionAction $ \context ->
        foldM (appendNext context) (Right ()) events
  where
    appendNext context previous event =
        case previous of
            Left err -> pure (Left err)
            Right () -> appendOne context event

    appendOne context (Here (DomainEvent article)) = do
        created <- newIdentifier
        case created of
            Left err -> pure (Left err)
            Right identifier ->
                append context
                    OutboxRecord
                        { identifier = eventIdentifierText identifier
                        , article = article
                        , eventKind = "ArticleReadyToPublish"
                        , payload = payloadText identifier (articleIdentifierText article)
                        , expectedRevision = Nothing
                        }
    appendOne _ (There impossible) = case impossible of {}

    payloadText :: EventIdentifier -> Text -> Text
    payloadText identifier article =
        decodeUtf8 $ Lazy.toStrict $ encode $ object
            [ "identifier" .= eventIdentifierText identifier
            , "occurredAt" .= command.timestamp
            , "actor" .= actorText command.actor
            , "correlation" .= correlationIdentifierText command.correlation
            , "causation" .= fmap causationText command.causation
            , "event" .= article
            ]
