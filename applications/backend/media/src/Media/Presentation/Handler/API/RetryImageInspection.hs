module Media.Presentation.Handler.API.RetryImageInspection (
    RetryImageInspectionHandlerDependencies (..),
    RetryImageInspectionHandlerOutput (..),
    retryImageInspectionHandler,
) where

import Control.Monad.Except (throwError)
import Data.Text (Text)
import Media.Presentation.API (CorrelatedResponse)
import Media.Presentation.API.RetryImageInspection
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
    uploadAttemptIdentifierText,
 )
import "media" Media.UseCase.RetryImageInspection (
    RetryImageInspection,
    newRetryImageInspection,
 )

newtype RetryImageInspectionHandlerOutput = RetryImageInspectionHandlerOutput
    { uploadAttemptIdentifier :: UploadAttemptIdentifier
    }

data RetryImageInspectionHandlerDependencies = RetryImageInspectionHandlerDependencies
    { metadata :: MetadataDependencies
    , executeRetryImageInspection ::
        Command RetryImageInspection ->
        IO (Either DomainError RetryImageInspectionHandlerOutput)
    }

retryImageInspectionHandler ::
    RetryImageInspectionHandlerDependencies ->
    Maybe Text ->
    Maybe Text ->
    Text ->
    Handler env (CorrelatedResponse RetryImageInspectionResponse)
retryImageInspectionHandler dependencies actorHeader correlationHeader rawIdentifier = do
    (rawCommand, correlation) <-
        newCommand dependencies.metadata actorHeader correlationHeader rawIdentifier
    imageIdentifier <- decodeImageIdentifier correlation rawIdentifier
    let command =
            mapCommandPayload
                (const (newRetryImageInspection imageIdentifier))
                rawCommand
    output <-
        runDomainOperation
            correlation
            retryImageInspectionError
            (dependencies.executeRetryImageInspection command)
    pure
        ( addHeader
            correlation
            ( RetryImageInspectionResponse
                (uploadAttemptIdentifierText output.uploadAttemptIdentifier)
            )
        )

decodeImageIdentifier :: Text -> Text -> Handler env ImageIdentifier
decodeImageIdentifier correlation rawIdentifier =
    either
        (const (throwError (invalidRequestError correlation "invalid_image_identifier")))
        pure
        (imageIdentifierFromText rawIdentifier)

retryImageInspectionError :: Text -> DomainError -> ServerError
retryImageInspectionError correlation domainError =
    case domainError of
        InvariantViolation _ ->
            publicServerError
                400
                "The image identifier is invalid."
                "invalid_image_identifier"
                correlation
        AggregateNotFound _ ->
            publicServerError 404 "The image was not found." "image_not_found" correlation
        OperationNotAllowed _ ->
            publicServerError
                409
                "The image inspection cannot be retried in its current state."
                "image_inspection_cannot_be_retried"
                correlation
        ServiceUnavailable _ ->
            publicServerError
                503
                "The service is temporarily unavailable."
                "service_unavailable"
                correlation
        UnexpectedError _ ->
            publicServerError 500 "An unexpected error occurred." "unexpected_error" correlation
