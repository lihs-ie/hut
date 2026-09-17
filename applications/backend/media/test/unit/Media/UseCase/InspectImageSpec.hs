module Media.UseCase.InspectImageSpec (run) where

import Data.Either (isLeft)
import Data.Time (addUTCTime)
import Media.Domain.Image (
    Image (AwaitingUpload),
    ImageRejection (
        ImageDimensionsTooLarge,
        ImageFileTooLarge,
        ImageHasTooManyPixels,
        MalformedImage,
        UnsupportedImageFormat
    ),
    SourceImageFormat (
        SourceGIF,
        SourceHEIC,
        SourceJPEG,
        SourcePNG,
        SourceWebP
    ),
    beginImageInspection,
    foldAvailableImage,
    foldInspectingImage,
    foldRejectedImageUpload,
    gifAnimationPreserved,
    imageWithoutAnimation,
    newImageByteSize,
    newImageDimensions,
    newImageHeight,
    newImagePixelCount,
    newImageWidth,
    newSuccessfulImageInspection,
    orientationApplied,
    privateNormalizedSource,
    restartImageUpload,
    sensitiveMetadataRemoved,
    uploadAttemptIdentifierText,
 )
import Media.Domain.Image.Event (
    ImageBecameAvailablePayload (ImageBecameAvailablePayload),
    ImageUploadRejectedPayload (ImageUploadRejectedPayload),
    newImageBecameAvailable,
    newImageUploadRejected,
 )
import Media.TestSupport (
    assertEqual,
    awaitingDeclaration,
    baseTime,
    expectRight,
    failTest,
    successfulPngInspection,
    testAwaiting,
    testCommandAt,
    testImageIdentifier,
    testUploadAttemptIdentifier,
    validByteSize,
    validDimensions,
    validPixelCount,
    (<&&>),
 )
import Media.UseCase.InspectImage (
    foldInspectImageResult,
    inspectImage,
    inspectImageResult,
    newAcceptedInspection,
    newMalformedImageInspection,
    newUnsupportedImageInspection,
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (

 )
import Media.UseCase.Result qualified as UseCase (resultEvents)
import Media.UseCase.RetryImageInspection qualified as RetryInspection (

 )
import Shared.Domain.Error (createOperationNotAllowed)
import Shared.Domain.Event (
    DomainEvent (DomainEvent),
    Events (Events),
    OneOf (Here, There),
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ testStaleAttemptCannotStartInspection
            , testBeginImageInspection
            , testAcceptedInspectionCorrelation
            , testRejectedInspectionCorrelation
            , testGifRequiresPreservedAnimation
            , testInspectionPolicyRejectsOversizedImage
            , testInspectionPolicyRejectsOversizedDimensions
            , testInspectionPolicyRejectsExcessivePixelCount
            , testHeicInputIsAcceptedWithoutRetainingEvidence
            , testUnsupportedInspection
            ]

testStaleAttemptCannotStartInspection :: IO Bool
testStaleAttemptCannotStartInspection = do
    imageIdentifier <- testImageIdentifier 1
    staleAttempt <- testUploadAttemptIdentifier 2
    currentAttempt <- testUploadAttemptIdentifier 3
    original <- testAwaiting imageIdentifier staleAttempt
    inspected <- successfulPngInspection
    retried <-
        expectRight
            "awaiting upload can restart"
            (restartImageUpload currentAttempt (addUTCTime 60 baseTime) (AwaitingUpload original))
    inspectCommand <-
        testCommandAt
            (addUTCTime 90 baseTime)
            (newAcceptedInspection staleAttempt retried inspected)
    assertEqual
        "stale upload attempt is not converted into a domain rejection"
        ( Left
            ( createOperationNotAllowed
                "ImageInspection"
                ( "upload attempt is not current: expected "
                    <> uploadAttemptIdentifierText currentAttempt
                    <> ", actual "
                    <> uploadAttemptIdentifierText staleAttempt
                )
            )
        )
        (inspectImage inspectCommand)

testBeginImageInspection :: IO Bool
testBeginImageInspection = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    let startedAt = addUTCTime 30 baseTime
    inspecting <-
        expectRight
            "current attempt begins inspection"
            (beginImageInspection startedAt attempt awaiting)
    foldInspectingImage
        ( \actualIdentifier actualAttempt declaration requestedAt actualStartedAt ->
            assertEqual "inspection preserves identifier" imageIdentifier actualIdentifier
                <&&> assertEqual "inspection preserves attempt" attempt actualAttempt
                <&&> assertEqual
                    "inspection preserves declaration"
                    (awaitingDeclaration awaiting)
                    declaration
                <&&> assertEqual "inspection preserves request time" baseTime requestedAt
                <&&> assertEqual "inspection records start time" startedAt actualStartedAt
        )
        inspecting

testAcceptedInspectionCorrelation :: IO Bool
testAcceptedInspectionCorrelation = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    inspected <- successfulPngInspection
    command <- testCommandAt baseTime (newAcceptedInspection attempt awaiting inspected)
    concreteResult <- expectRight "current attempt is inspected" (inspectImage command)
    foldInspectImageResult
        ( \available (DomainEvent (ImageBecameAvailablePayload eventImage)) ->
            assertEqual "accepted event contains the resulting image" available eventImage
                <&&> foldAvailableImage
                    ( \actualIdentifier availableAt ->
                        assertEqual
                            "available image only retains its identifier"
                            imageIdentifier
                            actualIdentifier
                            <&&> assertEqual "availableAt comes from command" baseTime availableAt
                    )
                    available
                <&&> case UseCase.resultEvents (inspectImageResult concreteResult) of
                    Events [Here event] ->
                        assertEqual
                            "typed result contains exactly the accepted event"
                            (newImageBecameAvailable available)
                            event
                    _ -> failTest "accepted generic result has an invalid event count or type"
        )
        (\_ _ -> failTest "accepted inspection selected rejection branch")
        concreteResult

testRejectedInspectionCorrelation :: IO Bool
testRejectedInspectionCorrelation = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    command <- testCommandAt baseTime (newMalformedImageInspection attempt awaiting)
    concreteResult <- expectRight "current malformed upload is inspected" (inspectImage command)
    foldInspectImageResult
        (\_ _ -> failTest "malformed inspection selected accepted branch")
        ( \rejected (DomainEvent (ImageUploadRejectedPayload eventImage reason)) ->
            assertEqual "rejected event identifies image" imageIdentifier eventImage
                <&&> assertEqual "rejected event contains permanent reason" MalformedImage reason
                <&&> foldRejectedImageUpload
                    ( \actualIdentifier actualReason rejectedAt ->
                        assertEqual
                            "rejected state preserves identifier"
                            imageIdentifier
                            actualIdentifier
                            <&&> assertEqual
                                "rejected state preserves reason"
                                MalformedImage
                                actualReason
                            <&&> assertEqual "rejectedAt comes from command" baseTime rejectedAt
                    )
                    rejected
                <&&> case UseCase.resultEvents (inspectImageResult concreteResult) of
                    Events [There (Here event)] ->
                        assertEqual
                            "typed result contains exactly the rejection event"
                            (newImageUploadRejected imageIdentifier MalformedImage)
                            event
                    _ -> failTest "rejected generic result has an invalid event count or type"
        )
        concreteResult

testGifRequiresPreservedAnimation :: IO Bool
testGifRequiresPreservedAnimation = do
    dimensions <- validDimensions
    bytes <- validByteSize
    pixels <- validPixelCount
    let invalid =
            newSuccessfulImageInspection
                SourceGIF
                dimensions
                bytes
                pixels
                orientationApplied
                sensitiveMetadataRemoved
                privateNormalizedSource
                imageWithoutAnimation
        valid =
            newSuccessfulImageInspection
                SourceGIF
                dimensions
                bytes
                pixels
                orientationApplied
                sensitiveMetadataRemoved
                privateNormalizedSource
                gifAnimationPreserved
    assertEqual "GIF without animation proof is rejected" True (isLeft invalid)
        <&&> assertEqual
            "GIF animation proof has no frame-count policy"
            True
            (either (const False) (const True) valid)

testInspectionPolicyRejectsOversizedImage :: IO Bool
testInspectionPolicyRejectsOversizedImage = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    dimensions <- validDimensions
    bytes <- expectRight "oversized byte count is measurable" (newImageByteSize (21 * 1024 * 1024))
    pixels <- validPixelCount
    inspected <-
        expectRight
            "normalized oversized image is inspection evidence"
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
    command <- testCommandAt baseTime (newAcceptedInspection attempt awaiting inspected)
    result <-
        expectRight
            "oversized current attempt reaches domain assessment"
            (inspectImage command)
    foldInspectImageResult
        (\_ _ -> failTest "oversized image was accepted")
        ( \_ (DomainEvent (ImageUploadRejectedPayload _ reason)) ->
            case reason of
                ImageFileTooLarge _ _ -> pure True
                _ -> failTest "oversized image produced the wrong rejection"
        )
        result

testInspectionPolicyRejectsOversizedDimensions :: IO Bool
testInspectionPolicyRejectsOversizedDimensions = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    width <- expectRight "oversized width is positive" (newImageWidth 12001)
    height <- expectRight "valid height" (newImageHeight 800)
    bytes <- validByteSize
    pixels <- validPixelCount
    inspected <-
        expectRight
            "oversized dimensions are inspection evidence"
            ( newSuccessfulImageInspection
                SourceJPEG
                (newImageDimensions width height)
                bytes
                pixels
                orientationApplied
                sensitiveMetadataRemoved
                privateNormalizedSource
                imageWithoutAnimation
            )
    command <- testCommandAt baseTime (newAcceptedInspection attempt awaiting inspected)
    result <- expectRight "oversized dimensions are assessed" (inspectImage command)
    foldInspectImageResult
        (\_ _ -> failTest "oversized dimensions were accepted")
        ( \_ (DomainEvent (ImageUploadRejectedPayload _ reason)) ->
            case reason of
                ImageDimensionsTooLarge _ _ -> pure True
                _ -> failTest "oversized dimensions produced the wrong rejection"
        )
        result

testInspectionPolicyRejectsExcessivePixelCount :: IO Bool
testInspectionPolicyRejectsExcessivePixelCount = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    dimensions <- validDimensions
    bytes <- validByteSize
    pixels <- expectRight "large pixel count is positive" (newImagePixelCount 100000001)
    inspected <-
        expectRight
            "large pixel count is inspection evidence"
            ( newSuccessfulImageInspection
                SourceWebP
                dimensions
                bytes
                pixels
                orientationApplied
                sensitiveMetadataRemoved
                privateNormalizedSource
                imageWithoutAnimation
            )
    command <- testCommandAt baseTime (newAcceptedInspection attempt awaiting inspected)
    result <- expectRight "large pixel count is assessed" (inspectImage command)
    foldInspectImageResult
        (\_ _ -> failTest "image with too many pixels was accepted")
        ( \_ (DomainEvent (ImageUploadRejectedPayload _ reason)) ->
            case reason of
                ImageHasTooManyPixels _ _ -> pure True
                _ -> failTest "large pixel count produced the wrong rejection"
        )
        result

testHeicInputIsAcceptedWithoutRetainingEvidence :: IO Bool
testHeicInputIsAcceptedWithoutRetainingEvidence = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    dimensions <- validDimensions
    bytes <- validByteSize
    pixels <- validPixelCount
    inspected <-
        expectRight
            "normalized HEIC is inspection evidence"
            ( newSuccessfulImageInspection
                SourceHEIC
                dimensions
                bytes
                pixels
                orientationApplied
                sensitiveMetadataRemoved
                privateNormalizedSource
                imageWithoutAnimation
            )
    command <- testCommandAt baseTime (newAcceptedInspection attempt awaiting inspected)
    result <- expectRight "HEIC current attempt is inspected" (inspectImage command)
    foldInspectImageResult
        ( \available _ ->
            foldAvailableImage
                ( \actualIdentifier availableAt ->
                    assertEqual
                        "HEIC source evidence is discarded after acceptance"
                        imageIdentifier
                        actualIdentifier
                        <&&> assertEqual "availableAt remains" baseTime availableAt
                )
                available
        )
        (\_ _ -> failTest "valid HEIC input was rejected")
        result

testUnsupportedInspection :: IO Bool
testUnsupportedInspection = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    command <- testCommandAt baseTime (newUnsupportedImageInspection attempt awaiting)
    result <- expectRight "unsupported upload is inspected" (inspectImage command)
    foldInspectImageResult
        (\_ _ -> failTest "unsupported upload was accepted")
        ( \_ (DomainEvent (ImageUploadRejectedPayload _ reason)) ->
            assertEqual
                "unsupported inspection preserves its rejection reason"
                UnsupportedImageFormat
                reason
        )
        result
