module Article.Worker.DO.AlarmSpec (run) where

import Article.Worker.DO.Alarm (runOutboxAlarmWith)
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
