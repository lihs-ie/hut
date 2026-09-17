module Media.Presentation.Handler.API.GetImageStatus (
    GetImageStatusHandlerDependencies (..),
    GetImageStatusHandlerOutput (..),
    getImageStatusHandler,
) where

import Control.Monad.Except (throwError)
import Data.Text (Text)
import Media.Presentation.API (CorrelatedResponse)
import Media.Presentation.API.GetImageStatus
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
    imageIdentifierFromText,
    imageIdentifierText,
 )
import "media" Media.UseCase.GetImageStatus (
    GetImageStatus,
    newGetImageStatus,
 )

data GetImageStatusHandlerOutput = GetImageStatusHandlerOutput
    { imageIdentifier :: ImageIdentifier
    , state :: Text
    }

data GetImageStatusHandlerDependencies = GetImageStatusHandlerDependencies
    { metadata :: MetadataDependencies
    , executeGetImageStatus ::
        Command GetImageStatus ->
        IO (Either DomainError GetImageStatusHandlerOutput)
    }

getImageStatusHandler ::
    GetImageStatusHandlerDependencies ->
    Maybe Text ->
    Maybe Text ->
    Text ->
    Handler env (CorrelatedResponse GetImageStatusResponse)
getImageStatusHandler dependencies actorHeader correlationHeader rawIdentifier = do
    (rawCommand, correlation) <-
        newCommand dependencies.metadata actorHeader correlationHeader rawIdentifier
    imageIdentifier <- decodeImageIdentifier correlation rawIdentifier
    let command =
            mapCommandPayload
                (const (newGetImageStatus imageIdentifier))
                rawCommand
    output <-
        runDomainOperation
            correlation
            getImageStatusError
            (dependencies.executeGetImageStatus command)
    pure
        ( addHeader
            correlation
            GetImageStatusResponse
                { imageIdentifier = imageIdentifierText output.imageIdentifier
                , state = output.state
                }
        )

decodeImageIdentifier :: Text -> Text -> Handler env ImageIdentifier
decodeImageIdentifier correlation rawIdentifier =
    either
        (const (throwError (invalidRequestError correlation "invalid_image_identifier")))
        pure
        (imageIdentifierFromText rawIdentifier)

getImageStatusError :: Text -> DomainError -> ServerError
getImageStatusError correlation domainError =
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
                "The image status is unavailable."
                "image_status_unavailable"
                correlation
        ServiceUnavailable _ ->
            publicServerError
                503
                "The service is temporarily unavailable."
                "service_unavailable"
                correlation
        UnexpectedError _ ->
            publicServerError 500 "An unexpected error occurred." "unexpected_error" correlation
