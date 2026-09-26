module Article.Worker.DO.Alarm (
    dispatchArticleOutboxAlarm,
    dispatchAllWith,
    runOutboxAlarmWith,
) where

import Cloudflare.Workers.Binding.DurableObject (
    DurableObjectStorage,
    doStorageSetAlarm,
 )
import Cloudflare.Workers.Binding.Queue (
    QueueProducer,
    queueSend,
    queueSendDefaultOptions,
 )
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Infrastructure.Article.DurableObject.Outbox (
    OutboxDispatchResult (..),
    dispatchPending,
 )
import Infrastructure.Article.DurableObject.MediaOutbox (dispatchPendingMedia)
import Infrastructure.Article.DurableObject.LogOutbox (dispatchPendingLogs)
import "shared" Shared.Domain.Error (DomainError)

dispatchArticleOutboxAlarm :: DurableObjectStorage -> QueueProducer -> QueueProducer -> IO ()
dispatchArticleOutboxAlarm storage generationQueue mediaQueue =
    runOutboxAlarmWith
        dispatchAll
        (doStorageSetAlarm storage)
        getCurrentTime
  where
    send queue message = do
        queueSend queue (Lazy.toStrict (encode message)) queueSendDefaultOptions
        pure (Right ())
    dispatchAll = dispatchAllWith
        (dispatchPending storage (send generationQueue))
        (dispatchPendingMedia storage (send mediaQueue))
        (dispatchPendingLogs storage)

dispatchAllWith ::
    IO (Either DomainError OutboxDispatchResult) ->
    IO (Either DomainError OutboxDispatchResult) ->
    IO (Either DomainError OutboxDispatchResult) ->
    IO (Either DomainError OutboxDispatchResult)
dispatchAllWith generation media logs = do
    firstResult <- generation
    secondResult <- media
    thirdResult <- logs
    pure $ do
        first <- firstResult
        second <- secondResult
        third <- thirdResult
        pure OutboxDispatchResult
            { deliveredCount = first.deliveredCount + second.deliveredCount + third.deliveredCount
            , hasPending = first.hasPending || second.hasPending || third.hasPending
            , deliveryFailure = case first.deliveryFailure of
                Just err -> Just err
                Nothing -> case second.deliveryFailure of
                    Just err -> Just err
                    Nothing -> third.deliveryFailure
            }

runOutboxAlarmWith ::
    IO (Either DomainError OutboxDispatchResult) ->
    (Integer -> IO ()) ->
    IO UTCTime ->
    IO ()
runOutboxAlarmWith dispatch schedule currentTime = do
    result <- dispatch
    let delay = case result of
            Left _ -> Just 30000
            Right outcome
                | not outcome.hasPending -> Nothing
                | Just _ <- outcome.deliveryFailure -> Just 30000
                | otherwise -> Just 1000
    forM_ delay $ \milliseconds -> do
        now <- currentTime
        schedule (floor (utcTimeToPOSIXSeconds now * 1000) + milliseconds)
    case result of
        Left err -> throwIO err
        Right outcome -> forM_ outcome.deliveryFailure throwIO
