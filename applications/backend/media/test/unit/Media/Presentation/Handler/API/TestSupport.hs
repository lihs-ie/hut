module Media.Presentation.Handler.API.TestSupport where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text (null)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Media.Presentation.API (CorrelatedResponse)
import Media.Presentation.API.GetImageStatus (
    GetImageStatusResponse (..),
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
    getImageStatusHandler,
 )
import Media.Presentation.Handler.API.Metadata (
    MetadataDependencies (MetadataDependencies),
 )
import Media.Presentation.Handler.API.RequestImageUpload (
    RequestImageUploadHandlerDependencies (RequestImageUploadHandlerDependencies),
    RequestImageUploadHandlerOutput (RequestImageUploadHandlerOutput),
    requestImageUploadHandler,
 )
import Media.Presentation.Handler.API.RetryImageInspection (
    RetryImageInspectionHandlerDependencies (RetryImageInspectionHandlerDependencies),
    RetryImageInspectionHandlerOutput (RetryImageInspectionHandlerOutput),
    retryImageInspectionHandler,
 )
import Media.Presentation.Handler.API.RetryImageUpload (
    RetryImageUploadHandlerDependencies (RetryImageUploadHandlerDependencies),
    RetryImageUploadHandlerOutput (RetryImageUploadHandlerOutput),
    retryImageUploadHandler,
 )
import Servant.API.ResponseHeaders (
    GetHeaders',
    Headers,
    getHeaders,
    getResponse,
 )
import Servant.Cloudflare.Workers.Error (
    ServerError,
    serverErrorHeaders,
    serverErrorMessage,
    serverErrorStatusCode,
 )
import Servant.Cloudflare.Workers.Handler (Handler (unHandler))
import Shared.Domain.Error (
    DomainError,
    createAggregateNotFound,
    createInvariantViolation,
    createOperationNotAllowed,
    createProcessingTargetChanged,
    createServiceUnavailable,
    createTransactionOutcomeUnknown,
    createUnexpectedError,
 )
import Shared.Domain.Identifier (ULID, newULID)
import "media" Media.Domain.Image (
    ImageIdentifier,
    UploadAttemptIdentifier,
    newImageIdentifier,
    newUploadAttemptIdentifier,
 )

named :: String -> IO Bool -> IO Bool
named label action = do
    passed <- action
    if passed then pure True else putStrLn ("FAILED: " <> label) >> pure False

uploadHandler ::
    Either DomainError RequestImageUploadHandlerOutput ->
    RequestImageUploadRequest ->
    Handler env (CorrelatedResponse RequestImageUploadResponse)
uploadHandler result =
    requestImageUploadHandler
        (RequestImageUploadHandlerDependencies metadataDependencies (const (pure result)))
        (Just actorValue)
        (Just supplied)

statusHandler ::
    Either DomainError GetImageStatusHandlerOutput ->
    Text ->
    Handler env (CorrelatedResponse GetImageStatusResponse)
statusHandler result =
    getImageStatusHandler
        (GetImageStatusHandlerDependencies metadataDependencies (const (pure result)))
        (Just actorValue)
        (Just supplied)

uploadRetryHandler ::
    Either DomainError RetryImageUploadHandlerOutput ->
    Text ->
    Handler env (CorrelatedResponse RetryImageUploadResponse)
uploadRetryHandler result =
    retryImageUploadHandler
        (RetryImageUploadHandlerDependencies metadataDependencies (const (pure result)))
        (Just actorValue)
        (Just supplied)

inspectionHandler ::
    Either DomainError RetryImageInspectionHandlerOutput ->
    Text ->
    Handler env (CorrelatedResponse RetryImageInspectionResponse)
inspectionHandler result =
    retryImageInspectionHandler
        (RetryImageInspectionHandlerDependencies metadataDependencies (const (pure result)))
        (Just actorValue)
        (Just supplied)

uploadWith ::
    MetadataDependencies ->
    Maybe Text ->
    Maybe Text ->
    Handler env (CorrelatedResponse RequestImageUploadResponse)
uploadWith metadata suppliedActor correlation =
    requestImageUploadHandler
        (RequestImageUploadHandlerDependencies metadata (const (pure (Right uploadOutput))))
        suppliedActor
        correlation
        uploadRequest

checkErrors ::
    (DomainError -> IO (Either ServerError value)) ->
    [(DomainError, Int, Text)] ->
    IO Bool
checkErrors invoke cases = do
    results <- traverse (\(err, status, code) -> isError status code supplied <$> invoke err) cases
    pure (and results)

isError :: Int -> Text -> Text -> Either ServerError value -> Bool
isError status code correlation result = case result of
    Left err ->
        serverErrorStatusCode err == status
            && lookup "X-Media-Error-Code" (serverErrorHeaders err) == Just code
            && lookup "X-Correlation-Identifier" (serverErrorHeaders err) == Just correlation
            && not (Text.null (serverErrorMessage err))
    Right _ -> False

correlated ::
    (Eq value, GetHeaders' headers) =>
    Either errorValue (Headers headers value) ->
    value ->
    ByteString ->
    Bool
correlated result expected correlation = case result of
    Right response ->
        getResponse response == expected
            && any ((== correlation) . snd) (getHeaders response)
    Left _ -> False

runHandler :: Handler () value -> IO (Either ServerError value)
runHandler action =
    runExceptT $
        runReaderT
            (runReaderT (unHandler action) ())
            (error "WorkersExecutionContext is unused")

metadataDependencies :: MetadataDependencies
metadataDependencies = MetadataDependencies (pure (Right fixedTime)) (pure (Right generated))

uploadRequest :: RequestImageUploadRequest
uploadRequest = RequestImageUploadRequest "image/png" 128 validDigest

uploadOutput :: RequestImageUploadHandlerOutput
uploadOutput =
    RequestImageUploadHandlerOutput imageIdentifier attemptIdentifier uploadURL fixedTime

uploadResponse :: RequestImageUploadResponse
uploadResponse = RequestImageUploadResponse validImage validAttempt uploadURL fixedTime

statusOutput :: GetImageStatusHandlerOutput
statusOutput = GetImageStatusHandlerOutput imageIdentifier "available"

uploadRetryOutput :: RetryImageUploadHandlerOutput
uploadRetryOutput =
    RetryImageUploadHandlerOutput imageIdentifier attemptIdentifier retryUploadURL fixedTime

uploadRetryResponse :: RetryImageUploadResponse
uploadRetryResponse = RetryImageUploadResponse validImage validAttempt retryUploadURL fixedTime

inspectionOutput :: RetryImageInspectionHandlerOutput
inspectionOutput = RetryImageInspectionHandlerOutput attemptIdentifier

imageIdentifier :: ImageIdentifier
imageIdentifier = newImageIdentifier (validULID validImageString)

attemptIdentifier :: UploadAttemptIdentifier
attemptIdentifier = newUploadAttemptIdentifier (validULID validAttemptString)

validULID :: Text -> ULID
validULID = either (error . show) id . newULID

actorValue :: Text
actorValue = "administrator-subject"

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 9 13) 0
validImage :: Text
validImage = "01K00000000000000000000000"
validImageString :: Text
validImageString = "01K00000000000000000000000"
validAttempt :: Text
validAttempt = "01K00000000000000000000001"
validAttemptString :: Text
validAttemptString = "01K00000000000000000000001"
generated :: Text
generated = "01K00000000000000000000002"
generatedBytes :: ByteString
generatedBytes = "01K00000000000000000000002" :: ByteString
supplied :: Text
supplied = "01K00000000000000000000003"
suppliedBytes :: ByteString
suppliedBytes = "01K00000000000000000000003" :: ByteString
emergency :: Text
emergency = "00000000000000000000000000"
uploadURL :: Text
uploadURL = "https://upload.example.test"
retryUploadURL :: Text
retryUploadURL = "https://upload.example.test/retry"
validDigest :: Text
validDigest = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

invariantError :: DomainError
invariantError = createInvariantViolation "Image" "invalid"
notFoundError :: DomainError
notFoundError = createAggregateNotFound "Image" validImage
notAllowedError :: DomainError
notAllowedError = createOperationNotAllowed "Image" "not allowed"
unavailableError :: DomainError
unavailableError = createServiceUnavailable "ImageRepository" "offline"
unexpectedError :: DomainError
unexpectedError = createUnexpectedError "Image" "unexpected"

unknownOutcomeError :: DomainError
unknownOutcomeError = createTransactionOutcomeUnknown "Transaction" "acknowledgement lost"

changedTargetError :: DomainError
changedTargetError = createProcessingTargetChanged "Image" "target changed"
