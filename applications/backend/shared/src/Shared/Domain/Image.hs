module Shared.Domain.Image (ImageIdentifier (..), newImageIdentifier) where

import Data.Text (Text)
import Data.Text qualified as Text
import Shared.Domain.Error (DomainError, createInvariantViolation)
import Shared.Domain.Identifier (ULID, ulidFromInteger)
import Text.Read (readMaybe)

newtype ImageIdentifier = ImageIdentifier ULID
    deriving stock (Show, Eq)

newImageIdentifier :: Text -> Either DomainError ImageIdentifier
newImageIdentifier value =
    case readMaybe (Text.unpack value) >>= either (const Nothing) Just . ulidFromInteger of
        Just identifier -> Right $ ImageIdentifier identifier
        Nothing ->
            Left $
                createInvariantViolation
                    "ImageIdentifier"
                    "value must be ULID integer format."
