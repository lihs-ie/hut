module Media.Presentation.Handler.API.Error (
    invalidRequestError,
    publicServerError,
    runDomainOperation,
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Servant.Cloudflare.Workers.Error (ServerError (ServerError))
import Servant.Cloudflare.Workers.Handler (Handler)
import Shared.Domain.Error (DomainError)

invalidRequestError :: Text -> Text -> ServerError
invalidRequestError correlation code =
    publicServerError 400 (invalidRequestMessage code) code correlation

invalidRequestMessage :: Text -> Text
invalidRequestMessage code =
    case code of
        "invalid_image_identifier" -> "The image identifier is invalid."
        "invalid_image_upload" -> "The image upload request is invalid."
        "invalid_correlation_identifier" -> "The correlation identifier is invalid."
        "invalid_actor" -> "The actor is invalid."
        _ -> "The request is invalid."

runDomainOperation ::
    Text ->
    (Text -> DomainError -> ServerError) ->
    IO (Either DomainError output) ->
    Handler env output
runDomainOperation correlation mapError operation = do
    result <- liftIO operation
    either
        (throwError . mapError correlation)
        pure
        result

publicServerError :: Int -> Text -> Text -> Text -> ServerError
publicServerError status message code correlation =
    ServerError
        status
        message
        [ ("X-Correlation-Identifier", correlation)
        , ("X-Media-Error-Code", code)
        , ("X-Media-Error-Message", message)
        ]
        Nothing
