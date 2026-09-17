module Media.Internal.UploadDestination (
    UploadDestination,
    UploadDestinationURL,
    newUploadDestination,
    newUploadDestinationURL,
    foldUploadDestination,
    uploadDestinationURLText,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Media.Domain.Image (ImageIdentifier, UploadAttemptIdentifier)
import Shared.Domain.Error (DomainError, createInvariantViolation)

newtype UploadDestinationURL = UploadDestinationURL Text
    deriving stock (Show, Eq)

data UploadDestination = UploadDestination
    { image :: ImageIdentifier
    , uploadAttempt :: UploadAttemptIdentifier
    , url :: UploadDestinationURL
    , expiresAt :: UTCTime
    }
    deriving stock (Show, Eq)

newUploadDestinationURL :: Text -> Either DomainError UploadDestinationURL
newUploadDestinationURL value
    | Text.null (Text.strip value) =
        Left
            ( createInvariantViolation
                "UploadDestinationURL"
                "value must not be blank"
            )
    | otherwise = Right (UploadDestinationURL value)

newUploadDestination ::
    ImageIdentifier ->
    UploadAttemptIdentifier ->
    UploadDestinationURL ->
    UTCTime ->
    UploadDestination
newUploadDestination = UploadDestination

foldUploadDestination ::
    (ImageIdentifier -> UploadAttemptIdentifier -> UploadDestinationURL -> UTCTime -> result) ->
    UploadDestination ->
    result
foldUploadDestination transform (UploadDestination image attempt url expiresAt) =
    transform image attempt url expiresAt

uploadDestinationURLText :: UploadDestinationURL -> Text
uploadDestinationURLText (UploadDestinationURL value) = value
