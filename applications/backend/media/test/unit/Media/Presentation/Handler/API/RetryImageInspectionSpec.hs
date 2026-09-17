module Media.Presentation.Handler.API.RetryImageInspectionSpec (run) where

import Media.Presentation.API.RetryImageInspection (
    RetryImageInspectionResponse (..),
 )
import Media.Presentation.Handler.API.TestSupport (
    checkErrors,
    correlated,
    inspectionHandler,
    inspectionOutput,
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
    validAttempt,
    validImage,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ named "inspection retry success" inspectionRetrySuccess
            , named "inspection retry invalid identifier" inspectionRetryInvalidIdentifier
            , named "inspection retry domain errors" inspectionRetryErrors
            ]

inspectionRetrySuccess :: IO Bool
inspectionRetrySuccess = do
    result <- runHandler $ inspectionHandler (Right inspectionOutput) validImage
    pure $ correlated result (RetryImageInspectionResponse validAttempt) suppliedBytes

inspectionRetryInvalidIdentifier :: IO Bool
inspectionRetryInvalidIdentifier = do
    result <- runHandler $ inspectionHandler (Right inspectionOutput) "bad"
    pure $ isError 400 "invalid_image_identifier" supplied result

inspectionRetryErrors :: IO Bool
inspectionRetryErrors =
    checkErrors
        (\err -> runHandler $ inspectionHandler (Left err) validImage)
        [ (invariantError, 400, "invalid_image_identifier")
        , (notFoundError, 404, "image_not_found")
        , (notAllowedError, 409, "image_inspection_cannot_be_retried")
        , (unavailableError, 503, "service_unavailable")
        , (unexpectedError, 500, "unexpected_error")
        ]
