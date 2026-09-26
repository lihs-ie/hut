module Media.Presentation.Handler.API.FindAvailableImages (
    FindAvailableImagesHandlerDependencies (..),
    findAvailableImagesHandler,
) where

import Control.Monad.Except (throwError)
import Data.Text (Text)
import Media.Presentation.API (CorrelatedResponse)
import Media.Presentation.API.FindAvailableImages
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
import "media" Media.UseCase.FindAvailableImages (
    FindAvailableImages,
    newFindAvailableImages,
 )

data FindAvailableImagesHandlerDependencies = FindAvailableImagesHandlerDependencies
    { metadata :: MetadataDependencies
    , executeFindAvailableImages ::
        Command FindAvailableImages -> IO (Either DomainError [ImageIdentifier])
    }

findAvailableImagesHandler ::
    FindAvailableImagesHandlerDependencies ->
    Maybe Text ->
    Maybe Text ->
    FindAvailableImagesRequest ->
    Handler env (CorrelatedResponse FindAvailableImagesResponse)
findAvailableImagesHandler dependencies actorHeader correlationHeader request = do
    (rawCommand, correlation) <-
        newCommand dependencies.metadata actorHeader correlationHeader request
    identifiers <- either
        (const (throwError (invalidRequestError correlation "invalid_image_identifier")))
        pure
        (traverse imageIdentifierFromText request.images)
    let command = mapCommandPayload
            (const (newFindAvailableImages identifiers))
            rawCommand
    available <- runDomainOperation
        correlation
        availabilityError
        (dependencies.executeFindAvailableImages command)
    pure (addHeader correlation
        (FindAvailableImagesResponse (map imageIdentifierText available)))

availabilityError :: Text -> DomainError -> ServerError
availabilityError correlation domainError =
    case domainError of
        ServiceUnavailable _ ->
            publicServerError 503 "The service is temporarily unavailable."
                "service_unavailable" correlation
        _ ->
            publicServerError 500 "An unexpected error occurred."
                "unexpected_error" correlation
