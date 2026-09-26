module Shared.Domain.Tag (TagIdentifier, newTagIdentifier, tagIdentifierText) where

import Data.Text (Text)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Identifier (ULID, newULID, ulidText)

newtype TagIdentifier = TagIdentifier ULID
    deriving stock (Show, Eq)

newTagIdentifier :: Text -> Either DomainError TagIdentifier
newTagIdentifier = fmap TagIdentifier . newULID

tagIdentifierText :: TagIdentifier -> Text
tagIdentifierText (TagIdentifier value) = ulidText value
