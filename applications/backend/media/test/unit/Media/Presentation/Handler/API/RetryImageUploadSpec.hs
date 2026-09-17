module Media.Presentation.Handler.API.RetryImageUploadSpec (run) where

import Media.Presentation.Handler.API.TestSupport (
    checkErrors,
    correlated,
    invariantError,
    isError,
    named,
    notAllowedError,
    notFoundError,
    runHandler,
    supplied,
    suppliedBytes,
    unavailableError,
    unexpectedError,
    uploadRetryHandler,
    uploadRetryOutput,
    uploadRetryResponse,
    validImage,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ named "upload retry success" uploadRetrySuccess
            , named "upload retry invalid identifier" uploadRetryInvalidIdentifier
            , named "upload retry domain errors" uploadRetryErrors
            ]

uploadRetrySuccess :: IO Bool
uploadRetrySuccess = do
    result <- runHandler $ uploadRetryHandler (Right uploadRetryOutput) validImage
    pure $ correlated result uploadRetryResponse suppliedBytes

uploadRetryInvalidIdentifier :: IO Bool
uploadRetryInvalidIdentifier = do
    result <- runHandler $ uploadRetryHandler (Right uploadRetryOutput) "bad"
    pure $ isError 400 "invalid_image_identifier" supplied result

uploadRetryErrors :: IO Bool
uploadRetryErrors =
    checkErrors
        (\err -> runHandler $ uploadRetryHandler (Left err) validImage)
        [ (invariantError, 400, "invalid_image_upload")
        , (notFoundError, 404, "image_not_found")
        , (notAllowedError, 409, "image_upload_cannot_be_retried")
        , (unavailableError, 503, "service_unavailable")
        , (unexpectedError, 500, "unexpected_error")
        ]
