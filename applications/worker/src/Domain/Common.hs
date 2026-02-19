module Domain.Common
  ( Timeline (..),
  )
where

import Data.Time (UTCTime)

data Timeline = Timeline
  { createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show)
