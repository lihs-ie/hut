module Infrastructure.Article.DurableObject.AlarmScheduleSpec (run) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Infrastructure.Article.DurableObject.AlarmSchedule (scheduleAlarmIfEarlierWith)
import TestSupport (check)

run :: IO ()
run = do
    alarm <- newIORef Nothing
    let schedule = writeIORef alarm . Just
        scheduleSooner = scheduleAlarmIfEarlierWith (readIORef alarm) schedule
    scheduleSooner 1000
    check "missing alarm is scheduled" =<< ((== Just 1000) <$> readIORef alarm)
    scheduleSooner 2000
    check "later work keeps the earlier alarm" =<< ((== Just 1000) <$> readIORef alarm)
    scheduleSooner 1000
    check "equal alarm is unchanged" =<< ((== Just 1000) <$> readIORef alarm)
    scheduleSooner 500
    check "earlier work advances the alarm" =<< ((== Just 500) <$> readIORef alarm)
