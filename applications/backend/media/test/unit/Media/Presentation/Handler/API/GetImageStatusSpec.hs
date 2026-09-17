module Media.Presentation.Handler.API.GetImageStatusSpec (run) where

import Media.Presentation.API.GetImageStatus (
    GetImageStatusResponse (..),
 )
import Media.Presentation.Handler.API.TestSupport (
    checkErrors,
    correlated,
    invariantError,
    isError,
    named,
    notAllowedError,
    notFoundError,
    runHandler,
    statusHandler,
    statusOutput,
    supplied,
    suppliedBytes,
    unavailableError,
    unexpectedError,
    validImage,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ named "status success" statusSuccess
            , named "status invalid identifier" statusInvalidIdentifier
            , named "status domain errors" statusErrors
            ]

statusSuccess :: IO Bool
statusSuccess = do
    result <- runHandler $ statusHandler (Right statusOutput) validImage
    pure $ correlated result (GetImageStatusResponse validImage "available") suppliedBytes

statusInvalidIdentifier :: IO Bool
statusInvalidIdentifier = do
    result <- runHandler $ statusHandler (Right statusOutput) "bad"
    pure $ isError 400 "invalid_image_identifier" supplied result

statusErrors :: IO Bool
statusErrors =
    checkErrors
        (\err -> runHandler $ statusHandler (Left err) validImage)
        [ (invariantError, 400, "invalid_image_identifier")
        , (notFoundError, 404, "image_not_found")
        , (notAllowedError, 409, "image_status_unavailable")
        , (unavailableError, 503, "service_unavailable")
        , (unexpectedError, 500, "unexpected_error")
        ]
