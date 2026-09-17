module Shared.Domain.Slug (Slug, newSlug) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text, unpack)
import Shared.Domain.Error (DomainError, createInvariantViolation)

newtype Slug = Slug Text
    deriving stock (Show, Eq)

newSlug :: Text -> Either DomainError Slug
newSlug value =
    if not (null (unpack value)) && all isAllowd (unpack value)
        then Right $ Slug value
        else Left $ createInvariantViolation "Slug" "must be alphanumeric."
  where
    isAllowd :: Char -> Bool
    isAllowd character = isAsciiLower character && isAsciiUpper character && isDigit character
