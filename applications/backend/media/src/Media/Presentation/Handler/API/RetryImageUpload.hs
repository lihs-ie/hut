module Media.Presentation.Handler.API.RetryImageUpload (
    RetryImageUploadHandlerDependencies (..),
    RetryImageUploadHandlerOutput (..),
    retryImageUploadHandler,
) where

import Control.Monad.Except (throwError)
import Data.Text (Text)
import Data.Time (UTCTime)
import Media.Presentation.API (CorrelatedResponse)
import Media.Presentation.API.RetryImageUpload
import Media.Presentation.Handler.API.Error (
    invalidRequestError,
    publicServerError,
    runDomainOperation,
 )
import Media.Presentation.Handler.API.Metadata
import Servant.API (addHeader)
import Servant.Cloudflare.Workers.Error (ServerError)
import Servant.Cloudflare.Workers.Handler (Handler)
import Shared.Domain.Error (DomainError (..))
import Shared.UseCase.Command (Command)
import "media" Media.Domain.Image (
    ImageIdentifier,
    UploadAttemptIdentifier,
    imageIdentifierFromText,
    imageIdentifierText,
    uploadAttemptIdentifierText,
 )
import "media" Media.UseCase.RetryImageUpload (
    RetryImageUpload,
    newRetryImageUpload,
 )

data RetryImageUploadHandlerOutput = RetryImageUploadHandlerOutput
    { imageIdentifier :: ImageIdentifier
    , uploadAttemptIdentifier :: UploadAttemptIdentifier
    , uploadDestination :: Text
    , expiresAt :: UTCTime
    }

data RetryImageUploadHandlerDependencies = RetryImageUploadHandlerDependencies
    { metadata :: MetadataDependencies
    , executeRetryImageUpload ::
        Command RetryImageUpload ->
        IO (Either DomainError RetryImageUploadHandlerOutput)
    }

retryImageUploadHandler ::
    RetryImageUploadHandlerDependencies ->
    Maybe Text ->
    Maybe Text ->
    Text ->
    Handler env (CorrelatedResponse RetryImageUploadResponse)
retryImageUploadHandler dependencies actorHeader correlationHeader rawIdentifier = do
    (rawCommand, correlation) <-
        newCommand dependencies.metadata actorHeader correlationHeader rawIdentifier
    imageIdentifier <- decodeImageIdentifier correlation rawIdentifier
    let command =
            mapCommandPayload
                (const (newRetryImageUpload imageIdentifier))
                rawCommand
    output <-
        runDomainOperation
            correlation
            retryImageUploadError
            (dependencies.executeRetryImageUpload command)
    pure (addHeader correlation (encodeResponse output))

decodeImageIdentifier :: Text -> Text -> Handler env ImageIdentifier
decodeImageIdentifier correlation rawIdentifier =
    either
        (const (throwError (invalidRequestError correlation "invalid_image_identifier")))
        pure
        (imageIdentifierFromText rawIdentifier)

retryImageUploadError :: Text -> DomainError -> ServerError
retryImageUploadError correlation domainError =
    case domainError of
        InvariantViolation _ ->
            publicServerError
                400
                "The image upload request is invalid."
                "invalid_image_upload"
                correlation
        AggregateNotFound _ ->
            publicServerError 404 "The image was not found." "image_not_found" correlation
        OperationNotAllowed _ ->
            publicServerError
                409
                "The image upload cannot be retried in its current state."
                "image_upload_cannot_be_retried"
                correlation
        ServiceUnavailable _ ->
            publicServerError
                503
                "The service is temporarily unavailable."
                "service_unavailable"
                correlation
        UnexpectedError _ ->
            publicServerError 500 "An unexpected error occurred." "unexpected_error" correlation

encodeResponse :: RetryImageUploadHandlerOutput -> RetryImageUploadResponse
encodeResponse output =
    RetryImageUploadResponse
        { imageIdentifier = imageIdentifierText output.imageIdentifier
        , uploadAttemptIdentifier =
            uploadAttemptIdentifierText output.uploadAttemptIdentifier
        , uploadDestination = output.uploadDestination
        , expiresAt = output.expiresAt
        }
