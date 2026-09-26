module Media.Presentation.Server.API.ServerSpec (run) where

import Cloudflare.Workers.HTTP (
    Method (GET, POST),
    Request (..),
    Response (responseBody, responseHeaders, responseStatus),
    ResponseBody (ResponseBodyBytes, ResponseBodyLazyBytes),
    Status (Status),
 )
import Cloudflare.Workers.Headers (headerLookup, headersFromList)
import Cloudflare.Workers.HostTestKit (phantomJSVal)
import Cloudflare.Workers.Reactor (
    WorkersExecutionContext (WorkersExecutionContext),
 )
import Cloudflare.Workers.URL (parseURL)
import Data.Aeson (FromJSON, decode, encode)
import Data.ByteString.Lazy qualified as LazyByteString (
    ByteString,
    fromStrict,
 )
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Media.Presentation.API (ErrorResponse (ErrorResponse))
import Media.Presentation.API.GetImageStatus (
    GetImageStatusResponse (..),
 )
import Media.Presentation.API.FindAvailableImages (
    FindAvailableImagesRequest (..),
    FindAvailableImagesResponse (..),
 )
import Media.Presentation.API.RequestImageUpload (
    RequestImageUploadRequest (RequestImageUploadRequest),
    RequestImageUploadResponse (RequestImageUploadResponse),
 )
import Media.Presentation.API.RetryImageInspection (
    RetryImageInspectionResponse (..),
 )
import Media.Presentation.API.RetryImageUpload (
    RetryImageUploadResponse (..),
 )
import Media.Presentation.Handler.API.GetImageStatus (
    GetImageStatusHandlerDependencies (GetImageStatusHandlerDependencies),
    GetImageStatusHandlerOutput (GetImageStatusHandlerOutput),
 )
import Media.Presentation.Handler.API.FindAvailableImages (
    FindAvailableImagesHandlerDependencies (FindAvailableImagesHandlerDependencies),
 )
import Media.Presentation.Handler.API.Metadata (
    MetadataDependencies (MetadataDependencies),
 )
import Media.Presentation.Handler.API.RequestImageUpload (
    RequestImageUploadHandlerDependencies (RequestImageUploadHandlerDependencies),
    RequestImageUploadHandlerOutput (RequestImageUploadHandlerOutput),
 )
import Media.Presentation.Handler.API.RetryImageInspection (
    RetryImageInspectionHandlerDependencies (RetryImageInspectionHandlerDependencies),
    RetryImageInspectionHandlerOutput (RetryImageInspectionHandlerOutput),
 )
import Media.Presentation.Handler.API.RetryImageUpload (
    RetryImageUploadHandlerDependencies (RetryImageUploadHandlerDependencies),
    RetryImageUploadHandlerOutput (RetryImageUploadHandlerOutput),
 )
import Media.Presentation.Server.API (
    APIServerDependencies (APIServerDependencies),
    mediaAPIServer,
    publicMessage,
    statusCodeToErrorCode,
 )
import Shared.Domain.Error (
    DomainError,
    createAggregateNotFound,
    createServiceUnavailable,
    createUnexpectedError,
 )
import Shared.Domain.Identifier (ULID, newULID)
import Shared.UseCase.Command (Command)
import "media" Media.Domain.Image (
    newImageIdentifier,
    newUploadAttemptIdentifier,
 )
import "media" Media.UseCase.GetImageStatus (
    GetImageStatus,
 )
import "media" Media.UseCase.RequestImageUpload (
    RequestImageUpload,
 )

run :: IO Bool
run = do
    results <-
        sequence
            [ named "missing actor header returns 400" missingActorHeader
            , named "invalid actor header returns 400" invalidActorHeader
            , named "POST /images returns 201" requestImageUploadRoute
            , named "missing image returns 404" getImageStatusNotFound
            , named "invalid correlation returns 400" invalidCorrelation
            , named "GET /images/:identifier returns 200" getImageStatusRoute
            , named "POST /images/availability returns available images" findAvailableImagesRoute
            , named "invalid availability identifier returns 400" invalidAvailabilityIdentifier
            , named "availability failure returns 503" availabilityFailure
            , named "POST upload-attempts returns 200" retryImageUploadRoute
            , named "POST inspection-retries returns 200" retryImageInspectionRoute
            , named "unknown route returns normalized 404" unknownRoute
            , named "wrong method returns normalized 405" wrongMethod
            , named "invalid JSON returns normalized 400" invalidJSON
            , named "missing content type returns normalized 415" missingContentType
            , named "public error mappings are stable" publicErrorMappings
            , named "error correlation fallbacks are stable" errorCorrelationFallbacks
            ]
    pure (and results)

named :: String -> IO Bool -> IO Bool
named label test = do
    passed <- test
    if passed
        then pure True
        else putStrLn ("FAILED: " <> label) >> pure False

missingActorHeader :: IO Bool
missingActorHeader = do
    let uploadRequest =
            (request POST "/images")
                { requestHeaders = headersFromList [("Content-Type", "application/json")]
                , requestBodyReaderField =
                    Just (\_ -> pure (Right (encode requestImageUploadBody)))
                }
    response <- runServer successfulDependencies uploadRequest
    let passed =
            statusCode response == 400
                && responseHeader correlationHeader response == Just generatedCorrelation
                && responseJSON response
                    == Just
                        ( ErrorResponse
                            "invalid_actor"
                            "The actor is invalid."
                        )
    if passed
        then pure True
        else do
            print
                ( statusCode response
                , responseHeader correlationHeader response
                , responseBytes response
                )
            pure False

invalidActorHeader :: IO Bool
invalidActorHeader = do
    let uploadRequest =
            requestWithActor
                ""
                POST
                "/images"
                [("Content-Type", "application/json")]
                (Just (encode requestImageUploadBody))
    response <- runServer successfulDependencies uploadRequest
    pure
        ( statusCode response == 400
            && responseHeader correlationHeader response == Just generatedCorrelation
            && responseJSON response
                == Just
                    ( ErrorResponse
                        "invalid_actor"
                        "The actor is invalid."
                    )
        )

requestImageUploadRoute :: IO Bool
requestImageUploadRoute = do
    routeCalled <- newIORef False
    let operation command = do
            command `seq` writeIORef routeCalled True
            pure (Right requestImageUploadOutput)
        dependencies =
            baseDependencies
                operation
                (const (pure unexpectedFailure))
        uploadRequest =
            internalRequest
                POST
                "/images"
                [ ("Content-Type", "application/json")
                , (correlationHeader, suppliedCorrelation)
                ]
                (Just (encode requestImageUploadBody))
    response <- runServer dependencies uploadRequest
    called <- readIORef routeCalled
    pure
        ( called
            && statusCode response == 201
            && responseHeader correlationHeader response == Just suppliedCorrelation
            && responseJSON response == Just requestImageUploadResponse
        )

getImageStatusNotFound :: IO Bool
getImageStatusNotFound = do
    let missing =
            Left (createAggregateNotFound "Image" validImageIdentifier)
        dependencies =
            baseDependencies
                (const (pure unexpectedFailure))
                (const (pure missing))
        statusRequest =
            internalRequest
                GET
                ("/images/" <> validImageIdentifier)
                [(correlationHeader, suppliedCorrelation)]
                Nothing
    response <- runServer dependencies statusRequest
    pure
        ( statusCode response == 404
            && responseHeader correlationHeader response == Just suppliedCorrelation
            && responseJSON response
                == Just (ErrorResponse "image_not_found" "The image was not found.")
        )

invalidCorrelation :: IO Bool
invalidCorrelation = do
    let statusRequest =
            internalRequest
                GET
                ("/images/" <> validImageIdentifier)
                [(correlationHeader, "not-a-ulid")]
                Nothing
    response <- runServer successfulDependencies statusRequest
    pure
        ( statusCode response == 400
            && responseHeader correlationHeader response == Just generatedCorrelation
            && responseJSON response
                == Just
                    ( ErrorResponse
                        "invalid_correlation_identifier"
                        "The correlation identifier is invalid."
                    )
        )

getImageStatusRoute :: IO Bool
getImageStatusRoute = do
    let dependencies =
            replaceGetImageStatus
                ( GetImageStatusHandlerDependencies
                    metadataDependencies
                    (const (pure (Right getImageStatusOutput)))
                )
                successfulDependencies
        statusRequest =
            internalRequest
                GET
                ("/images/" <> validImageIdentifier)
                [(correlationHeader, suppliedCorrelation)]
                Nothing
    response <- runServer dependencies statusRequest
    pure
        ( statusCode response == 200
            && responseHeader correlationHeader response == Just suppliedCorrelation
            && responseJSON response == Just getImageStatusResponse
        )

findAvailableImagesRoute :: IO Bool
findAvailableImagesRoute = do
    routeCalled <- newIORef False
    let operation command = do
            command `seq` writeIORef routeCalled True
            pure (Right [newImageIdentifier imageULID])
        dependencies = replaceFindAvailableImages
            (FindAvailableImagesHandlerDependencies metadataDependencies operation)
            successfulDependencies
        availabilityRequest = internalRequest POST "/images/availability"
            [("Content-Type", "application/json"), (correlationHeader, suppliedCorrelation)]
            (Just (encode (FindAvailableImagesRequest [validImageIdentifier])))
    response <- runServer dependencies availabilityRequest
    called <- readIORef routeCalled
    pure
        ( called
            && statusCode response == 200
            && responseHeader correlationHeader response == Just suppliedCorrelation
            && responseJSON response
                == Just (FindAvailableImagesResponse [validImageIdentifier])
        )

invalidAvailabilityIdentifier :: IO Bool
invalidAvailabilityIdentifier = do
    let availabilityRequest = internalRequest POST "/images/availability"
            [("Content-Type", "application/json")]
            (Just (encode (FindAvailableImagesRequest ["invalid"])))
    response <- runServer successfulDependencies availabilityRequest
    pure
        ( statusCode response == 400
            && responseJSON response
                == Just
                    (ErrorResponse "invalid_image_identifier" "The image identifier is invalid.")
        )

availabilityFailure :: IO Bool
availabilityFailure = do
    let dependencies = replaceFindAvailableImages
            (FindAvailableImagesHandlerDependencies metadataDependencies
                (const (pure (Left (createServiceUnavailable "Media" "database failed")))))
            successfulDependencies
        availabilityRequest = internalRequest POST "/images/availability"
            [("Content-Type", "application/json")]
            (Just (encode (FindAvailableImagesRequest [validImageIdentifier])))
    response <- runServer dependencies availabilityRequest
    pure
        ( statusCode response == 503
            && responseJSON response
                == Just
                    (ErrorResponse "service_unavailable" "The service is temporarily unavailable.")
        )

retryImageUploadRoute :: IO Bool
retryImageUploadRoute = do
    let dependencies =
            replaceRetryImageUpload
                ( RetryImageUploadHandlerDependencies
                    metadataDependencies
                    (const (pure (Right retryImageUploadOutput)))
                )
                successfulDependencies
        retryRequest =
            internalRequest
                POST
                ("/images/" <> validImageIdentifier <> "/upload-attempts")
                [(correlationHeader, suppliedCorrelation)]
                Nothing
    response <- runServer dependencies retryRequest
    pure
        ( statusCode response == 200
            && responseHeader correlationHeader response == Just suppliedCorrelation
            && responseJSON response == Just retryImageUploadResponse
        )

retryImageInspectionRoute :: IO Bool
retryImageInspectionRoute = do
    let dependencies =
            replaceRetryImageInspection
                ( RetryImageInspectionHandlerDependencies
                    metadataDependencies
                    (const (pure (Right retryImageInspectionOutput)))
                )
                successfulDependencies
        retryRequest =
            internalRequest
                POST
                ("/images/" <> validImageIdentifier <> "/inspection-retries")
                [(correlationHeader, suppliedCorrelation)]
                Nothing
    response <- runServer dependencies retryRequest
    pure
        ( statusCode response == 200
            && responseHeader correlationHeader response == Just suppliedCorrelation
            && responseJSON response == Just retryImageInspectionResponse
        )

unknownRoute :: IO Bool
unknownRoute = do
    response <-
        runServer
            successfulDependencies
            (internalRequest GET "/unknown" [] Nothing)
    pure
        ( statusCode response == 404
            && responseJSON response
                == Just (ErrorResponse "not_found" "The requested route was not found.")
        )

wrongMethod :: IO Bool
wrongMethod = do
    response <-
        runServer
            successfulDependencies
            (internalRequest POST ("/images/" <> validImageIdentifier) [] Nothing)
    pure
        ( statusCode response == 405
            && responseJSON response
                == Just (ErrorResponse "method_not_allowed" "The HTTP method is not allowed.")
        )

invalidJSON :: IO Bool
invalidJSON = do
    let uploadRequest =
            internalRequest
                POST
                "/images"
                [("Content-Type", "application/json")]
                (Just "not-json")
    response <- runServer successfulDependencies uploadRequest
    pure (statusCode response == 400 && responseHeader correlationHeader response /= Nothing)

missingContentType :: IO Bool
missingContentType = do
    let uploadRequest =
            internalRequest
                POST
                "/images"
                []
                (Just (encode requestImageUploadBody))
    response <- runServer successfulDependencies uploadRequest
    pure
        ( statusCode response == 415
            && responseJSON response
                == Just
                    ( ErrorResponse
                        "unsupported_media_type"
                        "The request media type is not supported."
                    )
        )

publicErrorMappings :: IO Bool
publicErrorMappings =
    pure
        ( fmap publicMessage publicCodes == publicMessages
            && fmap statusCodeToErrorCode statuses == statusCodes
        )
  where
    publicCodes =
        [ "invariant_violation"
        , "aggregate_not_found"
        , "operation_not_allowed"
        , "service_unavailable"
        , "invalid_image_identifier"
        , "invalid_image_upload"
        , "invalid_correlation_identifier"
        , "invalid_actor"
        , "invalid_request"
        , "method_not_allowed"
        , "unsupported_media_type"
        , "payload_too_large"
        , "not_found"
        , "unknown"
        ]
    publicMessages =
        [ "The request violates a domain invariant."
        , "The requested resource was not found."
        , "The requested operation is not allowed."
        , "The service is temporarily unavailable."
        , "The image identifier is invalid."
        , "The image upload request is invalid."
        , "The correlation identifier is invalid."
        , "The actor is invalid."
        , "The request is invalid."
        , "The HTTP method is not allowed."
        , "The request media type is not supported."
        , "The request payload is too large."
        , "The requested route was not found."
        , "An unexpected error occurred."
        ]
    statuses = [400, 404, 405, 409, 413, 415, 503, 500]
    statusCodes =
        [ "invalid_request"
        , "not_found"
        , "method_not_allowed"
        , "operation_not_allowed"
        , "payload_too_large"
        , "unsupported_media_type"
        , "service_unavailable"
        , "unexpected_error"
        ]

errorCorrelationFallbacks :: IO Bool
errorCorrelationFallbacks = do
    suppliedResponse <-
        runServer
            successfulDependencies
            (internalRequest GET "/unknown" [(correlationHeader, suppliedCorrelation)] Nothing)
    malformedResponse <-
        runServer
            (dependenciesWithCorrelation (pure (Right "not-a-ulid")))
            (internalRequest GET "/unknown" [] Nothing)
    failedResponse <-
        runServer
            (dependenciesWithCorrelation (pure unexpectedFailure))
            (internalRequest GET "/unknown" [] Nothing)
    pure
        ( responseHeader correlationHeader suppliedResponse == Just suppliedCorrelation
            && responseHeader correlationHeader malformedResponse == Just emergencyCorrelation
            && responseHeader correlationHeader failedResponse == Just emergencyCorrelation
        )

dependenciesWithCorrelation ::
    IO (Either DomainError Text) ->
    APIServerDependencies
dependenciesWithCorrelation source =
    replaceRequestImageUpload
        ( RequestImageUploadHandlerDependencies
            (MetadataDependencies (pure (Right fixedTime)) source)
            (const (pure (Right requestImageUploadOutput)))
        )
        successfulDependencies

runServer ::
    APIServerDependencies ->
    Request ->
    IO Response
runServer dependencies workerRequest =
    mediaAPIServer workerRequest dependencies executionContext

baseDependencies ::
    ( Command RequestImageUpload ->
      IO (Either DomainError RequestImageUploadHandlerOutput)
    ) ->
    ( Command GetImageStatus ->
      IO (Either DomainError GetImageStatusHandlerOutput)
    ) ->
    APIServerDependencies
baseDependencies requestOperation statusOperation =
    APIServerDependencies
        ( RequestImageUploadHandlerDependencies
            metadataDependencies
            requestOperation
        )
        ( RetryImageUploadHandlerDependencies
            metadataDependencies
            (const (pure unexpectedFailure))
        )
        ( GetImageStatusHandlerDependencies
            metadataDependencies
            statusOperation
        )
        ( FindAvailableImagesHandlerDependencies
            metadataDependencies
            (const (pure unexpectedFailure))
        )
        ( RetryImageInspectionHandlerDependencies
            metadataDependencies
            (const (pure unexpectedFailure))
        )

successfulDependencies :: APIServerDependencies
successfulDependencies =
    baseDependencies
        (const (pure (Right requestImageUploadOutput)))
        (const (pure unexpectedFailure))

replaceGetImageStatus ::
    GetImageStatusHandlerDependencies ->
    APIServerDependencies ->
    APIServerDependencies
replaceGetImageStatus replacement (APIServerDependencies upload retry _ availability inspection) =
    APIServerDependencies upload retry replacement availability inspection

replaceFindAvailableImages ::
    FindAvailableImagesHandlerDependencies ->
    APIServerDependencies ->
    APIServerDependencies
replaceFindAvailableImages replacement (APIServerDependencies upload retry status _ inspection) =
    APIServerDependencies upload retry status replacement inspection

replaceRequestImageUpload ::
    RequestImageUploadHandlerDependencies ->
    APIServerDependencies ->
    APIServerDependencies
replaceRequestImageUpload
    replacement
    (APIServerDependencies _ retry status availability inspection) =
    APIServerDependencies replacement retry status availability inspection

replaceRetryImageUpload ::
    RetryImageUploadHandlerDependencies ->
    APIServerDependencies ->
    APIServerDependencies
replaceRetryImageUpload
    replacement
    (APIServerDependencies upload _ status availability inspection) =
    APIServerDependencies upload replacement status availability inspection

replaceRetryImageInspection ::
    RetryImageInspectionHandlerDependencies ->
    APIServerDependencies ->
    APIServerDependencies
replaceRetryImageInspection replacement (APIServerDependencies upload retry status availability _) =
    APIServerDependencies upload retry status availability replacement

metadataDependencies :: MetadataDependencies
metadataDependencies =
    MetadataDependencies
        (pure (Right fixedTime))
        (pure (Right generatedCorrelation))

internalRequest ::
    Method ->
    Text ->
    [(Text, Text)] ->
    Maybe LazyByteString.ByteString ->
    Request
internalRequest = requestWithActor actor

requestWithActor ::
    Text ->
    Method ->
    Text ->
    [(Text, Text)] ->
    Maybe LazyByteString.ByteString ->
    Request
requestWithActor suppliedActor method path extraHeaders body =
    (request method path)
        { requestHeaders =
            headersFromList
                ((actorHeader, suppliedActor) : extraHeaders)
        , requestBodyReaderField = fmap (\bytes _ -> pure (Right bytes)) body
        }

request :: Method -> Text -> Request
request method path =
    Request
        method
        ( case parseURL ("https://media.example.test" <> path) of
            Just url -> url
            Nothing -> error "invalid fixed test URL"
        )
        Nothing
        (headersFromList [])
        Nothing
        Nothing

executionContext :: WorkersExecutionContext
executionContext = WorkersExecutionContext phantomJSVal

statusCode :: Response -> Int
statusCode response =
    case response.responseStatus of
        Status code -> code

responseHeader :: Text -> Response -> Maybe Text
responseHeader name response = headerLookup name response.responseHeaders

responseJSON :: (FromJSON value) => Response -> Maybe value
responseJSON = decode . responseBytes

responseBytes :: Response -> LazyByteString.ByteString
responseBytes response =
    case response.responseBody of
        ResponseBodyBytes bytes -> LazyByteString.fromStrict bytes
        ResponseBodyLazyBytes bytes -> bytes
        _ -> error "expected buffered response"

unexpectedFailure :: Either DomainError value
unexpectedFailure = Left (createUnexpectedError "ServerSpec" "unexpected operation")

requestImageUploadBody :: RequestImageUploadRequest
requestImageUploadBody =
    RequestImageUploadRequest
        "image/png"
        128
        validDigest

requestImageUploadOutput :: RequestImageUploadHandlerOutput
requestImageUploadOutput =
    RequestImageUploadHandlerOutput
        (newImageIdentifier imageULID)
        (newUploadAttemptIdentifier uploadAttemptULID)
        "https://upload.example.test"
        fixedTime

requestImageUploadResponse :: RequestImageUploadResponse
requestImageUploadResponse =
    RequestImageUploadResponse
        validImageIdentifier
        validUploadAttemptIdentifier
        "https://upload.example.test"
        fixedTime

getImageStatusOutput :: GetImageStatusHandlerOutput
getImageStatusOutput =
    GetImageStatusHandlerOutput (newImageIdentifier imageULID) "available"

getImageStatusResponse :: GetImageStatusResponse
getImageStatusResponse = GetImageStatusResponse validImageIdentifier "available"

retryImageUploadOutput :: RetryImageUploadHandlerOutput
retryImageUploadOutput =
    RetryImageUploadHandlerOutput
        (newImageIdentifier imageULID)
        (newUploadAttemptIdentifier uploadAttemptULID)
        "https://upload.example.test/retry"
        fixedTime

retryImageUploadResponse :: RetryImageUploadResponse
retryImageUploadResponse =
    RetryImageUploadResponse
        validImageIdentifier
        validUploadAttemptIdentifier
        "https://upload.example.test/retry"
        fixedTime

retryImageInspectionOutput :: RetryImageInspectionHandlerOutput
retryImageInspectionOutput =
    RetryImageInspectionHandlerOutput (newUploadAttemptIdentifier uploadAttemptULID)

retryImageInspectionResponse :: RetryImageInspectionResponse
retryImageInspectionResponse =
    RetryImageInspectionResponse validUploadAttemptIdentifier

actor :: Text
actor = "administrator-subject"

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 9 13) 0

imageULID :: ULID
imageULID = validULID "01K00000000000000000000000"

uploadAttemptULID :: ULID
uploadAttemptULID = validULID "01K00000000000000000000001"

validULID :: Text -> ULID
validULID = either (error . show) id . newULID

validImageIdentifier :: Text
validImageIdentifier = "01K00000000000000000000000"

validUploadAttemptIdentifier :: Text
validUploadAttemptIdentifier = "01K00000000000000000000001"

generatedCorrelation :: Text
generatedCorrelation = "01K00000000000000000000002"

emergencyCorrelation :: Text
emergencyCorrelation = "00000000000000000000000000"

suppliedCorrelation :: Text
suppliedCorrelation = "01K00000000000000000000003"

correlationHeader :: Text
correlationHeader = "X-Correlation-Identifier"

actorHeader :: Text
actorHeader = "X-Hut-Actor"

validDigest :: Text
validDigest = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
