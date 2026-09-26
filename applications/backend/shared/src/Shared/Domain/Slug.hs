module Shared.Domain.Slug (Slug, newSlug, slugText) where

import Data.Text (Text)
import Data.Text qualified as Text
import Shared.Domain.Error (DomainError, createInvariantViolation)

newtype Slug = Slug Text
    deriving stock (Show, Eq)

newSlug :: Text -> Either DomainError Slug
newSlug value
    | all validSegment (Text.splitOn "-" value) = Right (Slug value)
    | otherwise =
        Left
            ( createInvariantViolation
                "Slug"
                "use lowercase ASCII letters and digits separated by single hyphens"
            )
  where
    validSegment segment = not (Text.null segment) && Text.all validCharacter segment
    validCharacter character =
        (character >= 'a' && character <= 'z')
            || (character >= '0' && character <= '9')

slugText :: Slug -> Text
slugText (Slug value) = value
