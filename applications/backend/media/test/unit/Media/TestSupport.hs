module Media.TestSupport where

import Data.IORef (IORef, modifyIORef')
import Data.Text qualified as Text (replicate)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Media.Domain.Image (
    AvailableImage,
    AwaitingUploadImage,
    Image (AwaitingUpload),
    ImageByteSize,
    ImageDimensions,
    ImageIdentifier,
    ImagePixelCount,
    ImageUploadDeclaration,
    RejectedImageUpload,
    SourceImageFormat (SourcePNG),
    SuccessfulImageInspection,
    UploadAttemptIdentifier,
    foldAwaitingUploadImage,
    imageWithoutAnimation,
    newAwaitingUploadImage,
    newDeclaredImageContentType,
    newImageByteSize,
    newImageDimensions,
    newImageHeight,
    newImageIdentifier,
    newImagePixelCount,
    newImageSha256,
    newImageUploadDeclaration,
    newImageWidth,
    newSuccessfulImageInspection,
    newUploadAttemptIdentifier,
    orientationApplied,
    privateNormalizedSource,
    sensitiveMetadataRemoved,
 )
import Media.UseCase.InspectImage (
    foldInspectImageResult,
    inspectImage,
    newAcceptedInspection,
    newMalformedImageInspection,
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (
    RequestImageUpload,
    UploadDestination,
    UploadDestinationURL,
    foldRequestImageUpload,
    foldUploadDestination,
    newRequestImageUpload,
    newUploadDestinationURL,
 )
import Media.UseCase.RetryImageInspection qualified as RetryInspection (

 )
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (Events (Events))
import Shared.Domain.Identifier (ULID, ulidFromInteger)
import Shared.UseCase.Command (Command (Command))
import Shared.UseCase.Context (
    newActor,
    newCorrelationIdentifier,
 )
import System.Exit (exitFailure)

acceptedImage :: IO AvailableImage
acceptedImage = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    inspected <- successfulPngInspection
    command <- testCommandAt baseTime (newAcceptedInspection attempt awaiting inspected)
    result <- expectRight "accepted image fixture reaches inspection" (inspectImage command)
    foldInspectImageResult
        (\available _ -> pure available)
        (\_ _ -> failFixture "accepted image fixture was rejected")
        result

rejectedImage :: IO RejectedImageUpload
rejectedImage = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    command <- testCommandAt baseTime (newMalformedImageInspection attempt awaiting)
    result <- expectRight "rejected image fixture reaches inspection" (inspectImage command)
    foldInspectImageResult
        (\_ _ -> failFixture "rejected image fixture was accepted")
        (\rejected _ -> pure rejected)
        result

testAwaiting :: ImageIdentifier -> UploadAttemptIdentifier -> IO AwaitingUploadImage
testAwaiting imageIdentifier attempt = do
    payload <- validRequestPayload
    pure
        ( newAwaitingUploadImage
            imageIdentifier
            attempt
            (requestDeclaration payload)
            baseTime
        )

validRequestPayload :: IO RequestUpload.RequestImageUpload
validRequestPayload = do
    contentType <-
        expectRight
            "valid declared content type"
            (newDeclaredImageContentType "image/png")
    bytes <- validByteSize
    digest <- expectRight "valid SHA-256" (newImageSha256 (Text.replicate 64 "a"))
    pure (RequestUpload.newRequestImageUpload contentType bytes digest)

requestDeclaration :: RequestUpload.RequestImageUpload -> ImageUploadDeclaration
requestDeclaration = RequestUpload.foldRequestImageUpload newImageUploadDeclaration

awaitingDeclaration :: AwaitingUploadImage -> ImageUploadDeclaration
awaitingDeclaration = foldAwaitingUploadImage (\_ _ declaration _ -> declaration)

successfulPngInspection :: IO SuccessfulImageInspection
successfulPngInspection = do
    dimensions <- validDimensions
    bytes <- validByteSize
    pixels <- validPixelCount
    expectRight
        "valid PNG inspection"
        ( newSuccessfulImageInspection
            SourcePNG
            dimensions
            bytes
            pixels
            orientationApplied
            sensitiveMetadataRemoved
            privateNormalizedSource
            imageWithoutAnimation
        )

validDimensions :: IO ImageDimensions
validDimensions = do
    width <- expectRight "valid width" (newImageWidth 1200)
    height <- expectRight "valid height" (newImageHeight 800)
    pure (newImageDimensions width height)

validByteSize :: IO ImageByteSize
validByteSize = expectRight "valid byte size" (newImageByteSize 2048)

validPixelCount :: IO ImagePixelCount
validPixelCount = expectRight "valid pixel count" (newImagePixelCount 960000)

testImageIdentifier :: Integer -> IO ImageIdentifier
testImageIdentifier raw = newImageIdentifier <$> testUlid raw

testUploadAttemptIdentifier :: Integer -> IO UploadAttemptIdentifier
testUploadAttemptIdentifier raw = newUploadAttemptIdentifier <$> testUlid raw

testUlid :: Integer -> IO ULID
testUlid raw = expectRight "valid ULID fixture" (ulidFromInteger raw)

validUploadDestinationURL :: IO RequestUpload.UploadDestinationURL
validUploadDestinationURL =
    expectRight
        "valid upload destination URL"
        (RequestUpload.newUploadDestinationURL "https://upload.example.test/image")

destinationIdentity ::
    RequestUpload.UploadDestination ->
    (ImageIdentifier, UploadAttemptIdentifier, UTCTime)
destinationIdentity =
    RequestUpload.foldUploadDestination
        ( \imageIdentifier uploadAttempt _ expiresAt ->
            (imageIdentifier, uploadAttempt, expiresAt)
        )

expectedRetriedImage ::
    AwaitingUploadImage ->
    UploadAttemptIdentifier ->
    UTCTime ->
    Image
expectedRetriedImage awaiting attempt requestedAt =
    AwaitingUpload
        ( foldAwaitingUploadImage
            ( \identifier _ declaration _ ->
                newAwaitingUploadImage identifier attempt declaration requestedAt
            )
            awaiting
        )

recordCall :: IORef [String] -> String -> IO (Either DomainError ())
recordCall calls label = do
    modifyIORef' calls (<> [label])
    pure (Right ())

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2026 9 12) 0

testCommandAt :: UTCTime -> payload -> IO (Command payload)
testCommandAt timestamp payload = do
    actor <- expectRight "valid actor" (newActor "administrator")
    correlation <-
        expectRight
            "valid correlation"
            (newCorrelationIdentifier "00000000000000000000000001")
    pure (Command payload timestamp actor correlation Nothing)

hasNoEvents :: Events '[] -> Bool
hasNoEvents (Events values) = null values

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO Bool
assertEqual label expected actual
    | expected == actual = putStrLn ("PASS: " <> label) >> pure True
    | otherwise = do
        putStrLn ("FAIL: " <> label <> ": expected " <> show expected <> ", got " <> show actual)
        pure False

assertLeftEqual ::
    (Eq errorValue, Show errorValue) =>
    String ->
    errorValue ->
    Either errorValue value ->
    IO Bool
assertLeftEqual label expected (Left actual) = assertEqual label expected actual
assertLeftEqual label _ (Right _) = failTest (label <> ": expected Left, got Right")

infixr 3 <&&>

(<&&>) :: IO Bool -> IO Bool -> IO Bool
left <&&> right = (&&) <$> left <*> right

failTest :: String -> IO Bool
failTest label = putStrLn ("FAIL: " <> label) >> pure False

expectRight :: (Show errorValue) => String -> Either errorValue value -> IO value
expectRight _ (Right value) = pure value
expectRight label (Left err) = do
    putStrLn ("FAIL: " <> label <> ": " <> show err)
    exitFailure

failFixture :: String -> IO value
failFixture label = putStrLn ("FAIL: " <> label) >> exitFailure
