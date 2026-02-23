module Aspects.Log
  ( LogEntry (..),
    LogLevel (..),
  )
where

import Data.Time (UTCTime)

data LogLevel = Info | Warn | Error deriving (Show, Eq)

data LogEntry = LogEntry
  { level :: LogLevel,
    message :: String,
    timestamp :: UTCTime
  }
  deriving (Show, Eq)
