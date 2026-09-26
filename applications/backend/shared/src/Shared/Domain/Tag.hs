module Shared.Domain.Tag (TagIdentifier, newTagIdentifier, tagIdentifierText) where

import Data.Text (Text)
import Data.Text qualified as Text
import Shared.Domain.Error (DomainError, createInvariantViolation)

newtype TagIdentifier = TagIdentifier Text
    deriving stock (Show, Eq)

newTagIdentifier :: Text -> Either DomainError TagIdentifier
newTagIdentifier value =
    if Text.null value || Text.length value > 255
        then Left $ createInvariantViolation "TagIdentifier" "Value length must be 1 ~ 255."
        else Right $ TagIdentifier value

tagIdentifierText :: TagIdentifier -> Text
tagIdentifierText (TagIdentifier value) = value
