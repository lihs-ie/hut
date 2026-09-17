module Shared.Domain.Date (
    Timeline,
    UnvalidatedTimeline (..),
    newTime,
    newTimeline,
    validateTimeline,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Shared.Domain.Error (DomainError, createInvariantViolation)

data Timeline = Timeline
    { createdAt :: UTCTime
    , updatedAt :: UTCTime
    }
    deriving stock (Show, Eq)

newTimeline :: UTCTime -> UTCTime -> Either DomainError Timeline
newTimeline createdAt updatedAt =
    if updatedAt < createdAt
        then Left $ createInvariantViolation "Timeline" "createdAt must be before updatedAt."
        else Right $ Timeline createdAt updatedAt

data UnvalidatedTimeline = UnvalidatedTimeline
    { createdAt :: Text
    , updatedAt :: Text
    }
    deriving stock (Show, Eq)

newTime :: Text -> Either DomainError UTCTime
newTime value =
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack value) of
        Just publishedAt -> Right publishedAt
        Nothing ->
            Left $
                createInvariantViolation
                    "PublishedAt"
                    "value must be ISO-8601 UTC format."

validateTimeline :: UnvalidatedTimeline -> Either DomainError Timeline
validateTimeline unvalidated = do
    createdAt <- newTime unvalidated.createdAt
    updatedAt <- newTime unvalidated.updatedAt
    newTimeline createdAt updatedAt
