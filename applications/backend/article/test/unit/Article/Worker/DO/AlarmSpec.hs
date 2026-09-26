module Article.Worker.DO.AlarmSpec (run) where

import Article.Worker.DO.Alarm (dispatchAllWith, runOutboxAlarmWith)
import Control.Exception (try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Infrastructure.Article.DurableObject.Outbox (OutboxDispatchResult (..))
import Shared.Domain.Error (DomainError, createServiceUnavailable)
import TestSupport (check, timestamp)

run :: IO ()
run = do
    skipsIdleAlarm
    reschedulesPendingBatch
    reschedulesFailedDelivery
    combinesDispatchers

millis :: Integer
millis = floor (utcTimeToPOSIXSeconds (timestamp 1) * 1000)

skipsIdleAlarm :: IO ()
skipsIdleAlarm = do
    called <- newIORef False
    runOutboxAlarmWith
        (pure (Right (OutboxDispatchResult 0 False Nothing)))
        (\_ -> writeIORef called True)
        (pure (timestamp 1))
    check "idle outbox does not reschedule" . not =<< readIORef called

reschedulesPendingBatch :: IO ()
reschedulesPendingBatch = do
    scheduled <- newIORef Nothing
    runOutboxAlarmWith
        (pure (Right (OutboxDispatchResult 100 True Nothing)))
        (writeIORef scheduled . Just)
        (pure (timestamp 1))
    check "pending outbox schedules next batch" . (== Just (millis + 1000))
        =<< readIORef scheduled

reschedulesFailedDelivery :: IO ()
reschedulesFailedDelivery = do
    scheduled <- newIORef Nothing
    let unavailable = createServiceUnavailable "ArticleQueue" "unavailable"
    result <- try @DomainError $
        runOutboxAlarmWith
            (pure (Right (OutboxDispatchResult 0 True (Just unavailable))))
            (writeIORef scheduled . Just)
            (pure (timestamp 1))
    check "failed dispatch remains an error" (result == Left unavailable)
    check "failed dispatch schedules retry" . (== Just (millis + 30000))
        =<< readIORef scheduled
    sqlFailure <- try @DomainError $
        runOutboxAlarmWith
            (pure (Left unavailable))
            (writeIORef scheduled . Just)
            (pure (timestamp 1))
    check "storage failure remains an error" (sqlFailure == Left unavailable)

combinesDispatchers :: IO ()
combinesDispatchers = do
    let first = OutboxDispatchResult 1 False Nothing
        second = OutboxDispatchResult 2 True Nothing
        third = OutboxDispatchResult 3 False Nothing
        unavailable = createServiceUnavailable "Queue" "down"
    combined <- dispatchAllWith
        (pure (Right first)) (pure (Right second)) (pure (Right third))
    check "all dispatchers contribute to pending state"
        (combined == Right (OutboxDispatchResult 6 True Nothing))
    failed <- dispatchAllWith
        (pure (Right first)) (pure (Right second{deliveryFailure = Just unavailable}))
        (pure (Right third))
    check "media delivery failure propagates" (failed == Right
        (OutboxDispatchResult 6 True (Just unavailable)))
    storage <- dispatchAllWith
        (pure (Right first)) (pure (Left unavailable)) (pure (Right third))
    check "storage failure propagates" (storage == Left unavailable)
    logPending <- dispatchAllWith
        (pure (Right first)) (pure (Right third))
        (pure (Right second))
    check "log outbox pending reschedules" (logPending == Right
        (OutboxDispatchResult 6 True Nothing))
    logFailure <- dispatchAllWith
        (pure (Right first)) (pure (Right third))
        (pure (Right second{deliveryFailure = Just unavailable}))
    check "log delivery failure propagates" (logFailure == Right
        (OutboxDispatchResult 6 True (Just unavailable)))
    generationFailure <- dispatchAllWith
        (pure (Right first{deliveryFailure = Just unavailable}))
        (pure (Right second)) (pure (Right third))
    check "generation failure has precedence" (generationFailure == Right
        (OutboxDispatchResult 6 True (Just unavailable)))
