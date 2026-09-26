module Infrastructure.Article.DurableObject.AlarmSchedule (
    scheduleOutboxAlarmSoon,
    scheduleOutboxAlarmAt,
    scheduleAlarmIfEarlierWith,
) where

import Cloudflare.Workers.Binding.DurableObject (
    DurableObjectStorage,
    doStorageGetAlarm,
    doStorageSetAlarm,
 )
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

scheduleOutboxAlarmSoon :: DurableObjectStorage -> IO ()
scheduleOutboxAlarmSoon storage = do
    now <- getCurrentTime
    scheduleOutboxAlarmAt storage
        (floor (utcTimeToPOSIXSeconds now * 1000) + 1000)

scheduleOutboxAlarmAt :: DurableObjectStorage -> Integer -> IO ()
scheduleOutboxAlarmAt storage =
    scheduleAlarmIfEarlierWith
        (doStorageGetAlarm storage)
        (doStorageSetAlarm storage)

scheduleAlarmIfEarlierWith ::
    IO (Maybe Integer) -> (Integer -> IO ()) -> Integer -> IO ()
scheduleAlarmIfEarlierWith current schedule requested = do
    existing <- current
    case existing of
        Just earlier | earlier <= requested -> pure ()
        _ -> schedule requested
