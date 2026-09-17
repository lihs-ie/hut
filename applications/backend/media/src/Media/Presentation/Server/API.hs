module Media.Presentation.Server.API (
    APIServerDependencies (..),
    mediaAPIServer,
    publicMessage,
    statusCodeToErrorCode,
) where

import Cloudflare.Workers.Entrypoint.Fetch (FetchHandler)
import Cloudflare.Workers.HTTP (
    Request (requestHeaders),
    Response (..),
    ResponseBody (ResponseBodyLazyBytes),
    Status (Status),
 )
import Cloudflare.Workers.Headers (
    headerInsert,
    headerLookup,
    headersFromList,
    headersToList,
 )
import Control.Exception (SomeException, catch)
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Media.Presentation.API
import Media.Presentation.Handler.API.GetImageStatus
import Media.Presentation.Handler.API.Metadata (
    MetadataDependencies (generateCorrelationIdentifier),
    canonicalCorrelation,
 )
import Media.Presentation.Handler.API.RequestImageUpload
import Media.Presentation.Handler.API.RetryImageInspection
import Media.Presentation.Handler.API.RetryImageUpload
import Servant.Cloudflare.Workers.Generic (AsWorker)
import Servant.Cloudflare.Workers.Server (
    Context (EmptyContext),
    serveWithContext,
 )

data APIServerDependencies = APIServerDependencies
    { requestImageUpload :: RequestImageUploadHandlerDependencies
    , retryImageUpload :: RetryImageUploadHandlerDependencies
    , getImageStatus :: GetImageStatusHandlerDependencies
    , retryImageInspection :: RetryImageInspectionHandlerDependencies
    }

mediaAPIServer :: FetchHandler APIServerDependencies
mediaAPIServer request dependencies context = do
    response <-
        serveWithContext
            (Proxy @MediaAPI)
            EmptyContext
            (mediaRoutes dependencies)
            request
            context
            ()
            `catch` \(_ :: SomeException) ->
                pure unexpectedResponse
    normalizeErrorResponse dependencies request response
        `catch` \(_ :: SomeException) ->
            pure emergencyErrorResponse

mediaRoutes ::
    APIServerDependencies ->
    Maybe Text ->
    Maybe Text ->
    MediaRoutes (AsWorker ())
mediaRoutes dependencies actor correlation =
    MediaRoutes
        { requestImageUpload =
            requestImageUploadHandler
                (requestImageUploadDependencies dependencies)
                actor
                correlation
        , retryImageUpload =
            retryImageUploadHandler
                (retryImageUploadDependencies dependencies)
                actor
                correlation
        , getImageStatus =
            getImageStatusHandler
                (getImageStatusDependencies dependencies)
                actor
                correlation
        , retryImageInspection =
            retryImageInspectionHandler
                (retryImageInspectionDependencies dependencies)
                actor
                correlation
        }

requestImageUploadDependencies ::
    APIServerDependencies ->
    RequestImageUploadHandlerDependencies
requestImageUploadDependencies dependencies =
    dependencies.requestImageUpload

retryImageUploadDependencies ::
    APIServerDependencies ->
    RetryImageUploadHandlerDependencies
retryImageUploadDependencies dependencies =
    dependencies.retryImageUpload

getImageStatusDependencies ::
    APIServerDependencies ->
    GetImageStatusHandlerDependencies
getImageStatusDependencies dependencies =
    dependencies.getImageStatus

retryImageInspectionDependencies ::
    APIServerDependencies ->
    RetryImageInspectionHandlerDependencies
retryImageInspectionDependencies dependencies =
    dependencies.retryImageInspection

normalizeErrorResponse ::
    APIServerDependencies ->
    Request ->
    Response ->
    IO Response
normalizeErrorResponse dependencies request response
    | responseStatusCode response < 400 = pure response
    | otherwise = do
        correlation <- errorCorrelation dependencies request response
        let (code, message) = publicError response
            publicBody = ErrorResponse code message
            publicHeaders =
                headerInsert
                    "X-Correlation-Identifier"
                    correlation
                    (headersFromList (publicResponseHeaders response))
        pure
            Response
                { responseStatus = response.responseStatus
                , responseHeaders = publicHeaders
                , responseBody = ResponseBodyLazyBytes (encode publicBody)
                }

errorCorrelation :: APIServerDependencies -> Request -> Response -> IO Text
errorCorrelation dependencies request response =
    case responseCorrelation of
        Just correlation -> pure correlation
        Nothing ->
            case requestCorrelation of
                Just correlation -> pure correlation
                Nothing -> generateCorrelation dependencies
  where
    responseCorrelation =
        headerLookup "X-Correlation-Identifier" response.responseHeaders
            >>= canonicalCorrelation
    requestCorrelation =
        headerLookup "X-Correlation-Identifier" request.requestHeaders
            >>= canonicalCorrelation

generateCorrelation :: APIServerDependencies -> IO Text
generateCorrelation dependencies = do
    generated <-
        dependencies.requestImageUpload.metadata.generateCorrelationIdentifier
    pure
        ( case generated of
            Right value -> fromMaybe emergencyCorrelation (canonicalCorrelation value)
            Left _ -> emergencyCorrelation
        )

publicError :: Response -> (Text, Text)
publicError response =
    ( publicCode
    , fromMaybe (publicMessage publicCode) internalMessage
    )
  where
    internalCode = headerLookup "X-Media-Error-Code" response.responseHeaders
    internalMessage = headerLookup "X-Media-Error-Message" response.responseHeaders
    fallbackCode = statusCodeToErrorCode (responseStatusCode response)
    publicCode = fromMaybe fallbackCode internalCode

publicMessage :: Text -> Text
publicMessage code =
    case code of
        "invariant_violation" -> "The request violates a domain invariant."
        "aggregate_not_found" -> "The requested resource was not found."
        "operation_not_allowed" -> "The requested operation is not allowed."
        "service_unavailable" -> "The service is temporarily unavailable."
        "invalid_image_identifier" -> "The image identifier is invalid."
        "invalid_image_upload" -> "The image upload request is invalid."
        "invalid_correlation_identifier" -> "The correlation identifier is invalid."
        "invalid_actor" -> "The actor is invalid."
        "invalid_request" -> "The request is invalid."
        "method_not_allowed" -> "The HTTP method is not allowed."
        "unsupported_media_type" -> "The request media type is not supported."
        "payload_too_large" -> "The request payload is too large."
        "not_found" -> "The requested route was not found."
        _ -> "An unexpected error occurred."

statusCodeToErrorCode :: Int -> Text
statusCodeToErrorCode status =
    case status of
        400 -> "invalid_request"
        404 -> "not_found"
        405 -> "method_not_allowed"
        409 -> "operation_not_allowed"
        413 -> "payload_too_large"
        415 -> "unsupported_media_type"
        503 -> "service_unavailable"
        _ -> "unexpected_error"

publicResponseHeaders :: Response -> [(Text, Text)]
publicResponseHeaders response =
    ("Content-Type", "application/json;charset=utf-8")
        : filter
            (not . isPrivateHeader . fst)
            (headersToList response.responseHeaders)

isPrivateHeader :: Text -> Bool
isPrivateHeader headerName =
    Text.toCaseFold headerName
        `elem` [ "content-type"
               , "x-media-error-code"
               , "x-media-error-message"
               , "x-correlation-identifier"
               ]

responseStatusCode :: Response -> Int
responseStatusCode response =
    case response.responseStatus of
        Status status -> status

unexpectedResponse :: Response
unexpectedResponse =
    Response
        { responseStatus = Status 500
        , responseHeaders = headersFromList []
        , responseBody = ResponseBodyLazyBytes ""
        }

emergencyCorrelation :: Text
emergencyCorrelation = "00000000000000000000000000"

emergencyErrorResponse :: Response
emergencyErrorResponse =
    Response
        { responseStatus = Status 500
        , responseHeaders =
            headersFromList
                [ ("Content-Type", "application/json;charset=utf-8")
                , ("X-Correlation-Identifier", emergencyCorrelation)
                ]
        , responseBody =
            ResponseBodyLazyBytes
                (encode (ErrorResponse "unexpected_error" "An unexpected error occurred."))
        }
