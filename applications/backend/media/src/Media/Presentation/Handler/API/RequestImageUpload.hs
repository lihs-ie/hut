module Media.Presentation.Handler.API.RequestImageUpload (
    RequestImageUploadHandlerDependencies (..),
    RequestImageUploadHandlerOutput (..),
    requestImageUploadHandler,
) where

import Control.Monad.Except (throwError)
import Data.Text (Text)
import Data.Time (UTCTime)
import Media.Presentation.API (CorrelatedResponse)
import Media.Presentation.API.RequestImageUpload
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
    imageIdentifierText,
    newDeclaredImageContentType,
    newImageByteSize,
    newImageSha256,
    uploadAttemptIdentifierText,
 )
import "media" Media.UseCase.RequestImageUpload (
    RequestImageUpload,
    newRequestImageUpload,
 )

data RequestImageUploadHandlerOutput = RequestImageUploadHandlerOutput
    { imageIdentifier :: ImageIdentifier
    , uploadAttemptIdentifier :: UploadAttemptIdentifier
    , uploadDestination :: Text
    , expiresAt :: UTCTime
    }

data RequestImageUploadHandlerDependencies = RequestImageUploadHandlerDependencies
    { metadata :: MetadataDependencies
    , executeRequestImageUpload ::
        Command RequestImageUpload ->
        IO (Either DomainError RequestImageUploadHandlerOutput)
    }

requestImageUploadHandler ::
    RequestImageUploadHandlerDependencies ->
    Maybe Text ->
    Maybe Text ->
    RequestImageUploadRequest ->
    Handler env (CorrelatedResponse RequestImageUploadResponse)
requestImageUploadHandler dependencies actorHeader correlationHeader request = do
    (requestCommand, correlation) <-
        newCommand dependencies.metadata actorHeader correlationHeader request
    payload <- decodeRequest correlation request
    output <-
        runDomainOperation
            correlation
            requestImageUploadError
            ( dependencies.executeRequestImageUpload
                (mapCommandPayload (const payload) requestCommand)
            )
    pure (addHeader correlation (encodeResponse output))

decodeRequest ::
    Text ->
    RequestImageUploadRequest ->
    Handler env RequestImageUpload
decodeRequest correlation request = do
    contentType <-
        fromValidation correlation (newDeclaredImageContentType request.contentType)
    byteSize <- fromValidation correlation (newImageByteSize request.byteSize)
    digest <- fromValidation correlation (newImageSha256 request.sha256)
    pure (newRequestImageUpload contentType byteSize digest)

fromValidation :: Text -> Either error value -> Handler env value
fromValidation correlation =
    either
        (const (throwError (invalidRequestError correlation "invalid_image_upload")))
        pure

requestImageUploadError :: Text -> DomainError -> ServerError
requestImageUploadError correlation domainError =
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
                "The image upload is not allowed."
                "image_upload_not_allowed"
                correlation
        ServiceUnavailable _ ->
            publicServerError
                503
                "The service is temporarily unavailable."
                "service_unavailable"
                correlation
        TransactionOutcomeUnknown _ ->
            publicServerError
                500
                "The operation outcome could not be confirmed."
                "transaction_outcome_unknown"
                correlation
        ProcessingTargetChanged _ ->
            publicServerError
                409
                "The processing target has changed."
                "processing_target_changed"
                correlation
        UnexpectedError _ ->
            publicServerError 500 "An unexpected error occurred." "unexpected_error" correlation

encodeResponse :: RequestImageUploadHandlerOutput -> RequestImageUploadResponse
encodeResponse output =
    RequestImageUploadResponse
        { imageIdentifier = imageIdentifierText output.imageIdentifier
        , uploadAttemptIdentifier =
            uploadAttemptIdentifierText output.uploadAttemptIdentifier
        , uploadDestination = output.uploadDestination
        , expiresAt = output.expiresAt
        }
