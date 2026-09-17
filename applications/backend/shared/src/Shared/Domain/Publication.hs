module Shared.Domain.Publication (PublishStatus (..), newPublishStatus) where

import Data.Text (Text)
import Shared.Domain.Error (DomainError, createInvariantViolation)

data PublishStatus
    = Draft
    | Published
    | Private
    deriving stock (Show, Eq)

newPublishStatus :: Text -> Either DomainError PublishStatus
newPublishStatus "draft" = Right Draft
newPublishStatus "published" = Right Published
newPublishStatus "private" = Right Private
newPublishStatus unknown =
    Left $
        createInvariantViolation
            "PublishStatus"
            ("Unknown value: " <> unknown)
