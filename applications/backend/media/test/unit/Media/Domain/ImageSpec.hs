module Media.Domain.ImageSpec (run) where

import Data.Either (isLeft)
import Data.Text qualified as Text (empty, pack, replicate)
import Media.Domain.Image (
    Image (Available, AwaitingUpload, Inspecting, Rejected),
    ImageRejection (
        ImageByteSizeMismatch,
        ImageDimensionsTooLarge,
        ImageFileTooLarge,
        ImageHasTooManyPixels,
        ImageSha256Mismatch,
        ImageSha256Missing,
        UnsupportedImageFormat
    ),
    SourceImageFormat (SourceJPEG, SourcePNG),
    beginImageInspection,
    declaredImageContentTypeText,
    foldAvailableImage,
    foldImageDimensions,
    foldImageUploadDeclaration,
    foldRejectedImageUpload,
    foldSuccessfulImageInspection,
    gifAnimationPreserved,
    imageByteSizeInteger,
    imageHeightInteger,
    imageIdentifierFromText,
    imageIdentifierText,
    imageSha256Text,
    imageWidthInteger,
    imageWithoutAnimation,
    newAwaitingUploadImage,
    newDeclaredImageContentType,
    newImageByteSize,
    newImageDimensions,
    newImageHeight,
    newImagePixelCount,
    newImageSha256,
    newImageUploadDeclaration,
    newImageWidth,
    newMaximumImageByteSize,
    newMaximumImageDimensions,
    newMaximumImagePixelCount,
    newSuccessfulImageInspection,
    orientationApplied,
    privateNormalizedSource,
    restoreAvailableImage,
    restoreRejectedImageUpload,
    sensitiveMetadataRemoved,
    uploadAttemptIdentifierFromText,
    uploadAttemptIdentifierText,
    verifyImageUploadIntegrity,
 )
import Media.TestSupport (
    assertEqual,
    assertLeftEqual,
    baseTime,
    expectRight,
    successfulPngInspection,
    testImageIdentifier,
    testUploadAttemptIdentifier,
    validByteSize,
    validPixelCount,
    (<&&>),
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (
    newUploadDestinationURL,
 )
import Media.UseCase.RetryImageInspection qualified as RetryInspection (

 )
import Shared.Domain.Error (createInvariantViolation)
import Shared.UseCase.Context (
    newActor,
    newCorrelationIdentifier,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ testImageValueObjectFolds
            , testIdentifierRoundTrips
            , testInvalidValues
            , testUploadIntegrity
            ]

testUploadIntegrity :: IO Bool
testUploadIntegrity = do
    contentType <- expectRight "content type fixture" (newDeclaredImageContentType "image/png")
    declaredBytes <- validByteSize
    otherBytes <- expectRight "other byte size fixture" (newImageByteSize 2049)
    declaredDigest <- expectRight "declared digest fixture" (newImageSha256 (Text.replicate 64 "a"))
    otherDigest <- expectRight "other digest fixture" (newImageSha256 (Text.replicate 64 "b"))
    let declaration = newImageUploadDeclaration contentType declaredBytes declaredDigest
    assertEqual
        "matching R2 integrity metadata is accepted"
        (Right ())
        (verifyImageUploadIntegrity declaration declaredBytes (Just declaredDigest))
        <&&> assertEqual
            "byte-size mismatch is distinct"
            (Left ImageByteSizeMismatch)
            (verifyImageUploadIntegrity declaration otherBytes (Just declaredDigest))
        <&&> assertEqual
            "missing SHA-256 is distinct"
            (Left ImageSha256Missing)
            (verifyImageUploadIntegrity declaration declaredBytes Nothing)
        <&&> assertEqual
            "SHA-256 mismatch is distinct"
            (Left ImageSha256Mismatch)
            (verifyImageUploadIntegrity declaration declaredBytes (Just otherDigest))

testImageValueObjectFolds :: IO Bool
testImageValueObjectFolds = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    otherAttempt <- testUploadAttemptIdentifier 3
    contentType <- expectRight "content type fixture" (newDeclaredImageContentType "image/jpeg")
    byteSize <- validByteSize
    sha256 <- expectRight "digest fixture" (newImageSha256 (Text.replicate 64 "A"))
    width <- expectRight "width fixture" (newImageWidth 1200)
    height <- expectRight "height fixture" (newImageHeight 800)
    pixelCount <- validPixelCount
    let dimensions = newImageDimensions width height
        declaration = newImageUploadDeclaration contentType byteSize sha256
        available = restoreAvailableImage imageIdentifier baseTime
        rejected = restoreRejectedImageUpload imageIdentifier UnsupportedImageFormat baseTime
        maximumBytes = newMaximumImageByteSize byteSize
        maximumDimensions = newMaximumImageDimensions dimensions
        maximumPixels = newMaximumImagePixelCount pixelCount
        awaiting = newAwaitingUploadImage imageIdentifier attempt declaration baseTime
    inspected <- successfulPngInspection
    inspecting <-
        expectRight
            "inspection state fixture"
            (beginImageInspection baseTime attempt awaiting)
    let declarationValues =
            foldImageUploadDeclaration
                ( \declared bytes digest ->
                    ( declaredImageContentTypeText declared
                    , imageByteSizeInteger bytes
                    , imageSha256Text digest
                    )
                )
                declaration
        dimensionValues =
            foldImageDimensions
                ( \actualWidth actualHeight ->
                    (imageWidthInteger actualWidth, imageHeightInteger actualHeight)
                )
                dimensions
        inspectionValues =
            foldSuccessfulImageInspection
                ( \format actualDimensions bytes pixels orientation metadata source animation ->
                    ( format
                    , actualDimensions
                    , bytes
                    , pixels
                    , orientation
                    , metadata
                    , source
                    , animation
                    )
                )
                inspected
        restoredValues =
            ( foldAvailableImage (,) available
            , foldRejectedImageUpload (,,) rejected
            )
        wrappersAreInspectable =
            all
                (not . null)
                [show maximumBytes, show maximumDimensions, show maximumPixels]
        domainValuesAreInspectable =
            all
                (not . null)
                [ show imageIdentifier
                , show attempt
                , show contentType
                , show sha256
                , show declaration
                , show awaiting
                , show inspecting
                , show available
                , show rejected
                , show (AwaitingUpload awaiting)
                , show (Inspecting inspecting)
                , show (Available available)
                , show (Rejected rejected)
                , show SourceJPEG
                , show dimensions
                , show width
                , show height
                , show byteSize
                , show pixelCount
                , show (ImageFileTooLarge byteSize maximumBytes)
                , show (ImageDimensionsTooLarge dimensions maximumDimensions)
                , show (ImageHasTooManyPixels pixelCount maximumPixels)
                , show inspected
                , show orientationApplied
                , show sensitiveMetadataRemoved
                , show privateNormalizedSource
                , show gifAnimationPreserved
                ]
        orderedValuesRespectMagnitude =
            width < either (const width) id (newImageWidth 1201)
                && height <= height
                && byteSize >= byteSize
                && pixelCount > either (const pixelCount) id (newImagePixelCount 1)
    assertEqual
        "declaration fold exposes validated values"
        ("image/jpeg", 2048, Text.replicate 64 "a")
        declarationValues
        <&&> assertEqual "dimension fold exposes width and height" (1200, 800) dimensionValues
        <&&> assertEqual
            "inspection fold exposes every proof"
            ( SourcePNG
            , dimensions
            , byteSize
            , pixelCount
            , orientationApplied
            , sensitiveMetadataRemoved
            , privateNormalizedSource
            , imageWithoutAnimation
            )
            inspectionValues
        <&&> assertEqual
            "restored image states preserve persisted facts"
            ((imageIdentifier, baseTime), (imageIdentifier, UnsupportedImageFormat, baseTime))
            restoredValues
        <&&> assertEqual "maximum policy wrappers remain inspectable" True wrappersAreInspectable
        <&&> assertEqual
            "domain values support diagnostic rendering"
            True
            domainValuesAreInspectable
        <&&> assertEqual
            "numeric value objects preserve ordering"
            True
            orderedValuesRespectMagnitude
        <&&> assertEqual "distinct attempts are not equal" True (attempt /= otherAttempt)
        <&&> assertEqual
            "non-GIF input cannot claim preserved GIF animation"
            True
            ( isLeft
                ( newSuccessfulImageInspection
                    SourceJPEG
                    dimensions
                    byteSize
                    pixelCount
                    orientationApplied
                    sensitiveMetadataRemoved
                    privateNormalizedSource
                    gifAnimationPreserved
                )
            )

testIdentifierRoundTrips :: IO Bool
testIdentifierRoundTrips = do
    imageIdentifier <- testImageIdentifier 1
    uploadAttempt <- testUploadAttemptIdentifier 2
    assertEqual
        "image identifier text round-trips"
        (Right imageIdentifier)
        (imageIdentifierFromText (imageIdentifierText imageIdentifier))
        <&&> assertEqual
            "upload attempt text round-trips"
            (Right uploadAttempt)
            (uploadAttemptIdentifierFromText (uploadAttemptIdentifierText uploadAttempt))

testInvalidValues :: IO Bool
testInvalidValues =
    assertLeftEqual
        "zero width is invalid"
        (createInvariantViolation "ImageWidth" "value must be greater than zero")
        (newImageWidth 0)
        <&&> assertLeftEqual
            "negative height is invalid"
            (createInvariantViolation "ImageHeight" "value must be greater than zero")
            (newImageHeight (-1))
        <&&> assertLeftEqual
            "zero bytes is invalid"
            (createInvariantViolation "ImageByteSize" "value must be greater than zero")
            (newImageByteSize 0)
        <&&> assertLeftEqual
            "zero pixels is invalid"
            (createInvariantViolation "ImagePixelCount" "value must be greater than zero")
            (newImagePixelCount 0)
        <&&> assertLeftEqual
            "blank content type is invalid"
            (createInvariantViolation "DeclaredImageContentType" "value must not be blank")
            (newDeclaredImageContentType "  ")
        <&&> assertLeftEqual
            "non-SHA-256 text is invalid"
            ( createInvariantViolation
                "ImageSha256"
                "value must contain exactly 64 hexadecimal characters"
            )
            (newImageSha256 "abc")
        <&&> assertEqual
            "blank upload destination is invalid"
            True
            (isLeft (RequestUpload.newUploadDestinationURL "  "))
        <&&> assertEqual "blank actor is invalid" True (isLeft (newActor (Text.pack "  ")))
        <&&> assertEqual
            "blank correlation is invalid"
            True
            (isLeft (newCorrelationIdentifier Text.empty))
        <&&> assertEqual
            "lowercase image identifier is not canonical"
            True
            (isLeft (imageIdentifierFromText "01arz3ndektsv4rrffq69g5fav"))
        <&&> assertEqual
            "overflowing upload attempt identifier is not canonical"
            True
            (isLeft (uploadAttemptIdentifierFromText "81ARZ3NDEKTSV4RRFFQ69G5FAV"))
