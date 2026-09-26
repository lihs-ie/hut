module Shared.Domain.Excerpt (Excerpt, newExcerpt, excerptText) where

import Data.Text (Text)
import Data.Text qualified as Text
import Shared.Domain.Error (DomainError, createInvariantViolation)

newtype Excerpt = Excerpt Text
    deriving stock (Show, Eq)

newExcerpt :: Text -> Either DomainError Excerpt
newExcerpt value
    | Text.null (Text.strip value) || Text.length value > 200 =
        Left (createInvariantViolation "Excerpt" "length must be between 1 and 200")
    | otherwise = Right (Excerpt value)

excerptText :: Excerpt -> Text
excerptText (Excerpt value) = value
