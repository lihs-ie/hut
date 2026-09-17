module Media.Presentation.Handler.API.RequestImageUploadSpec (run) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Media.Presentation.API.RequestImageUpload (
    RequestImageUploadRequest (byteSize, contentType, sha256),
 )
import Media.Presentation.Handler.API.RequestImageUpload (
    RequestImageUploadHandlerDependencies (RequestImageUploadHandlerDependencies),
    requestImageUploadHandler,
 )
import Media.Presentation.Handler.API.TestSupport (
    actorValue,
    checkErrors,
    correlated,
    fixedTime,
    invariantError,
    isError,
    metadataDependencies,
    named,
    notAllowedError,
    notFoundError,
    runHandler,
    supplied,
    suppliedBytes,
    unavailableError,
    unexpectedError,
    uploadHandler,
    uploadOutput,
    uploadRequest,
    uploadResponse,
 )
import Shared.UseCase.Command (
    Command (..),
    actorText,
    correlationIdentifierText,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ named "upload success and command metadata" uploadSuccess
            , named "upload request validation" uploadValidation
            , named "upload domain errors" uploadErrors
            ]

uploadSuccess :: IO Bool
uploadSuccess = do
    captured <- newIORef Nothing
    let dependencies =
            RequestImageUploadHandlerDependencies metadataDependencies $ \command -> do
                writeIORef captured (Just command)
                pure (Right uploadOutput)
    result <-
        runHandler
            (requestImageUploadHandler dependencies (Just actorValue) (Just supplied) uploadRequest)
    command <- readIORef captured
    pure $ case command of
        Just value ->
            value.timestamp == fixedTime
                && actorText value.actor == actorValue
                && correlationIdentifierText value.correlation == supplied
                && value.causation == Nothing
                && correlated result uploadResponse suppliedBytes
        Nothing -> False

uploadValidation :: IO Bool
uploadValidation = do
    let requests =
            [ uploadRequest{contentType = "   "}
            , uploadRequest{byteSize = 0}
            , uploadRequest{sha256 = "bad"}
            ]
    results <- traverse (runHandler . uploadHandler (Right uploadOutput)) requests
    pure $ all (isError 400 "invalid_image_upload" supplied) results

uploadErrors :: IO Bool
uploadErrors =
    checkErrors
        (\err -> runHandler $ uploadHandler (Left err) uploadRequest)
        [ (invariantError, 400, "invalid_image_upload")
        , (notFoundError, 404, "image_not_found")
        , (notAllowedError, 409, "image_upload_not_allowed")
        , (unavailableError, 503, "service_unavailable")
        , (unexpectedError, 500, "unexpected_error")
        ]
