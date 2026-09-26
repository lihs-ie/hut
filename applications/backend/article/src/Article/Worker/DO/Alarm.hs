module Article.Worker.DO.Alarm (
    dispatchArticleOutboxAlarm,
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
import "shared" Shared.Domain.Error (DomainError)

dispatchArticleOutboxAlarm :: DurableObjectStorage -> QueueProducer -> IO ()
dispatchArticleOutboxAlarm storage queue =
    runOutboxAlarmWith
        (dispatchPending storage send)
        (doStorageSetAlarm storage)
        getCurrentTime
  where
    send message = do
        queueSend queue (Lazy.toStrict (encode message)) queueSendDefaultOptions
        pure (Right ())

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
