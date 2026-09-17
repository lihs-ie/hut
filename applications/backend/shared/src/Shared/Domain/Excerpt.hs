module Shared.Domain.Excerpt (Excerpt, newExcerpt) where

import Data.Text (Text)
import Data.Text qualified as Text
import Shared.Domain.Error (DomainError, createInvariantViolation)

newtype Excerpt = Excerpt Text
    deriving (Show, Eq)

newExcerpt :: Text -> Either DomainError Excerpt
newExcerpt value =
    if Text.null value || Text.length value > 100
        then Left $ createInvariantViolation "Excerpt" "value length must be between 1 and 100."
        else Right $ Excerpt value
