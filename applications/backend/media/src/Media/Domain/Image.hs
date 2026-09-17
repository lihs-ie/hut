module Media.Domain.Image (
    ImageIdentifier,
    UploadAttemptIdentifier,
    DeclaredImageContentType,
    ImageSha256,
    ImageUploadDeclaration,
    AwaitingUploadImage,
    InspectingImage,
    AvailableImage,
    RejectedImageUpload,
    Image (..),
    SourceImageFormat (..),
    ImageDimensions,
    ImageWidth,
    ImageHeight,
    ImageByteSize,
    ImagePixelCount,
    MaximumImageByteSize,
    MaximumImageDimensions,
    MaximumImagePixelCount,
    ImageRejection (..),
    SuccessfulImageInspection,
    OrientationApplied,
    SensitiveMetadataRemoved,
    PrivateNormalizedSource,
    AnimationHandling,
    newImageIdentifier,
    newUploadAttemptIdentifier,
    imageIdentifierText,
    uploadAttemptIdentifierText,
    imageIdentifierFromText,
    uploadAttemptIdentifierFromText,
    newDeclaredImageContentType,
    newImageSha256,
    newImageUploadDeclaration,
    verifyImageUploadIntegrity,
    newImageWidth,
    newImageHeight,
    newImageDimensions,
    newImageByteSize,
    newImagePixelCount,
    newMaximumImageByteSize,
    newMaximumImageDimensions,
    newMaximumImagePixelCount,
    orientationApplied,
    sensitiveMetadataRemoved,
    privateNormalizedSource,
    imageWithoutAnimation,
    gifAnimationPreserved,
    newSuccessfulImageInspection,
    newAwaitingUploadImage,
    restoreAvailableImage,
    restoreRejectedImageUpload,
    beginImageInspection,
    acceptImageInspection,
    rejectImageInspection,
    restartImageUpload,
    retryableInspectionAttempt,
    foldImageUploadDeclaration,
    foldAwaitingUploadImage,
    foldInspectingImage,
    foldAvailableImage,
    foldRejectedImageUpload,
    foldSuccessfulImageInspection,
    foldImageDimensions,
    declaredImageContentTypeText,
    imageSha256Text,
    imageByteSizeInteger,
    imageWidthInteger,
    imageHeightInteger,
    imagePixelCountInteger,
) where

import Data.Char (isHexDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Shared.Domain.Error (
    DomainError,
    createInvariantViolation,
    createOperationNotAllowed,
 )
import Shared.Domain.Identifier (ULID, newULID, ulidText)

newtype ImageIdentifier = ImageIdentifier ULID
    deriving stock (Show, Eq)

newtype UploadAttemptIdentifier = UploadAttemptIdentifier ULID
    deriving stock (Show, Eq)

newtype DeclaredImageContentType = DeclaredImageContentType Text
    deriving stock (Show, Eq)

newtype ImageSha256 = ImageSha256 Text
    deriving stock (Show, Eq)

data ImageUploadDeclaration = ImageUploadDeclaration
    { contentType :: DeclaredImageContentType
    , byteSize :: ImageByteSize
    , sha256 :: ImageSha256
    }
    deriving stock (Show, Eq)

data AwaitingUploadImage = AwaitingUploadImage
    { identifier :: ImageIdentifier
    , uploadAttempt :: UploadAttemptIdentifier
    , declaration :: ImageUploadDeclaration
    , requestedAt :: UTCTime
    }
    deriving stock (Show, Eq)

data InspectingImage = InspectingImage
    { identifier :: ImageIdentifier
    , uploadAttempt :: UploadAttemptIdentifier
    , declaration :: ImageUploadDeclaration
    , requestedAt :: UTCTime
    , inspectionStartedAt :: UTCTime
    }
    deriving stock (Show, Eq)

data AvailableImage = AvailableImage
    { identifier :: ImageIdentifier
    , availableAt :: UTCTime
    }
    deriving stock (Show, Eq)

data RejectedImageUpload = RejectedImageUpload
    { identifier :: ImageIdentifier
    , rejection :: ImageRejection
    , rejectedAt :: UTCTime
    }
    deriving stock (Show, Eq)

data Image
    = AwaitingUpload AwaitingUploadImage
    | Inspecting InspectingImage
    | Available AvailableImage
    | Rejected RejectedImageUpload
    deriving stock (Show, Eq)

data SourceImageFormat = SourcePNG | SourceJPEG | SourceWebP | SourceGIF | SourceHEIC
    deriving stock (Show, Eq)

newtype ImageWidth = ImageWidth Integer
    deriving stock (Show, Eq, Ord)

newtype ImageHeight = ImageHeight Integer
    deriving stock (Show, Eq, Ord)

data ImageDimensions = ImageDimensions
    { width :: ImageWidth
    , height :: ImageHeight
    }
    deriving stock (Show, Eq)

newtype ImageByteSize = ImageByteSize Integer
    deriving stock (Show, Eq, Ord)

newtype ImagePixelCount = ImagePixelCount Integer
    deriving stock (Show, Eq, Ord)

newtype MaximumImageByteSize = MaximumImageByteSize ImageByteSize
    deriving stock (Show, Eq)

newtype MaximumImageDimensions = MaximumImageDimensions ImageDimensions
    deriving stock (Show, Eq)

newtype MaximumImagePixelCount = MaximumImagePixelCount ImagePixelCount
    deriving stock (Show, Eq)

data ImageRejection
    = UnsupportedImageFormat
    | MalformedImage
    | ImageByteSizeMismatch
    | ImageSha256Mismatch
    | ImageSha256Missing
    | ImageFileTooLarge ImageByteSize MaximumImageByteSize
    | ImageDimensionsTooLarge ImageDimensions MaximumImageDimensions
    | ImageHasTooManyPixels ImagePixelCount MaximumImagePixelCount
    deriving stock (Show, Eq)

data SuccessfulImageInspection = SuccessfulImageInspection
    { sourceFormat :: SourceImageFormat
    , dimensions :: ImageDimensions
    , byteSize :: ImageByteSize
    , totalPixelCount :: ImagePixelCount
    , orientation :: OrientationApplied
    , metadata :: SensitiveMetadataRemoved
    , normalizedSource :: PrivateNormalizedSource
    , animation :: AnimationHandling
    }
    deriving stock (Show, Eq)

data OrientationApplied = OrientationApplied
    deriving stock (Show, Eq)

data SensitiveMetadataRemoved = SensitiveMetadataRemoved
    deriving stock (Show, Eq)

data PrivateNormalizedSource = PrivateNormalizedSource
    deriving stock (Show, Eq)

data AnimationHandling = ImageWithoutAnimation | GifAnimationPreserved
    deriving stock (Show, Eq)

data ImageInspectionPolicy = ImageInspectionPolicy
    { maximumByteSize :: MaximumImageByteSize
    , maximumDimensions :: MaximumImageDimensions
    , maximumTotalPixelCount :: MaximumImagePixelCount
    }

newImageIdentifier :: ULID -> ImageIdentifier
newImageIdentifier = ImageIdentifier

newUploadAttemptIdentifier :: ULID -> UploadAttemptIdentifier
newUploadAttemptIdentifier = UploadAttemptIdentifier

imageIdentifierText :: ImageIdentifier -> Text
imageIdentifierText (ImageIdentifier value) = ulidText value

uploadAttemptIdentifierText :: UploadAttemptIdentifier -> Text
uploadAttemptIdentifierText (UploadAttemptIdentifier value) = ulidText value

imageIdentifierFromText :: Text -> Either DomainError ImageIdentifier
imageIdentifierFromText value =
    maybe
        (Left (createInvariantViolation "ImageIdentifier" "value must be a canonical ULID"))
        (Right . ImageIdentifier)
        (canonicalULID value)

uploadAttemptIdentifierFromText :: Text -> Either DomainError UploadAttemptIdentifier
uploadAttemptIdentifierFromText value =
    maybe
        ( Left
            ( createInvariantViolation
                "UploadAttemptIdentifier"
                "value must be a canonical ULID"
            )
        )
        (Right . UploadAttemptIdentifier)
        (canonicalULID value)

canonicalULID :: Text -> Maybe ULID
canonicalULID = either (const Nothing) Just . newULID

newDeclaredImageContentType :: Text -> Either DomainError DeclaredImageContentType
newDeclaredImageContentType value
    | Text.null (Text.strip value) = Left (nonEmptyValueError "DeclaredImageContentType")
    | otherwise = Right (DeclaredImageContentType value)

newImageSha256 :: Text -> Either DomainError ImageSha256
newImageSha256 value
    | Text.length value == 64 && Text.all isHexDigit value =
        Right (ImageSha256 (Text.toLower value))
    | otherwise =
        Left
            ( createInvariantViolation
                "ImageSha256"
                "value must contain exactly 64 hexadecimal characters"
            )

newImageUploadDeclaration ::
    DeclaredImageContentType -> ImageByteSize -> ImageSha256 -> ImageUploadDeclaration
newImageUploadDeclaration = ImageUploadDeclaration

verifyImageUploadIntegrity ::
    ImageUploadDeclaration -> ImageByteSize -> Maybe ImageSha256 -> Either ImageRejection ()
verifyImageUploadIntegrity
    (ImageUploadDeclaration _ declaredByteSize declaredSha256)
    actualByteSize
    actualSha256
        | actualByteSize /= declaredByteSize =
            Left ImageByteSizeMismatch
        | Nothing <- actualSha256 = Left ImageSha256Missing
        | Just digest <- actualSha256
        , digest /= declaredSha256 =
            Left ImageSha256Mismatch
        | otherwise = Right ()

newImageWidth :: Integer -> Either DomainError ImageWidth
newImageWidth value
    | value > 0 = Right (ImageWidth value)
    | otherwise = Left (positiveValueError "ImageWidth")

newImageHeight :: Integer -> Either DomainError ImageHeight
newImageHeight value
    | value > 0 = Right (ImageHeight value)
    | otherwise = Left (positiveValueError "ImageHeight")

newImageDimensions :: ImageWidth -> ImageHeight -> ImageDimensions
newImageDimensions = ImageDimensions

newImageByteSize :: Integer -> Either DomainError ImageByteSize
newImageByteSize value
    | value > 0 = Right (ImageByteSize value)
    | otherwise = Left (positiveValueError "ImageByteSize")

newImagePixelCount :: Integer -> Either DomainError ImagePixelCount
newImagePixelCount value
    | value > 0 = Right (ImagePixelCount value)
    | otherwise = Left (positiveValueError "ImagePixelCount")

newMaximumImageByteSize :: ImageByteSize -> MaximumImageByteSize
newMaximumImageByteSize = MaximumImageByteSize

newMaximumImageDimensions :: ImageDimensions -> MaximumImageDimensions
newMaximumImageDimensions = MaximumImageDimensions

newMaximumImagePixelCount :: ImagePixelCount -> MaximumImagePixelCount
newMaximumImagePixelCount = MaximumImagePixelCount

orientationApplied :: OrientationApplied
orientationApplied = OrientationApplied

sensitiveMetadataRemoved :: SensitiveMetadataRemoved
sensitiveMetadataRemoved = SensitiveMetadataRemoved

privateNormalizedSource :: PrivateNormalizedSource
privateNormalizedSource = PrivateNormalizedSource

imageWithoutAnimation :: AnimationHandling
imageWithoutAnimation = ImageWithoutAnimation

gifAnimationPreserved :: AnimationHandling
gifAnimationPreserved = GifAnimationPreserved

newSuccessfulImageInspection ::
    SourceImageFormat ->
    ImageDimensions ->
    ImageByteSize ->
    ImagePixelCount ->
    OrientationApplied ->
    SensitiveMetadataRemoved ->
    PrivateNormalizedSource ->
    AnimationHandling ->
    Either DomainError SuccessfulImageInspection
newSuccessfulImageInspection
    format
    inspectedDimensions
    inspectedByteSize
    inspectedPixelCount
    appliedOrientation
    removedMetadata
    privateSource
    animationHandling
        | format == SourceGIF && animationHandling /= GifAnimationPreserved =
            Left
                ( createInvariantViolation
                    "AnimationHandling"
                    "GIF animation must be preserved"
                )
        | format /= SourceGIF && animationHandling == GifAnimationPreserved =
            Left
                ( createInvariantViolation
                    "AnimationHandling"
                    "only GIF input can carry preserved GIF animation"
                )
        | otherwise =
            Right
                SuccessfulImageInspection
                    { sourceFormat = format
                    , dimensions = inspectedDimensions
                    , byteSize = inspectedByteSize
                    , totalPixelCount = inspectedPixelCount
                    , orientation = appliedOrientation
                    , metadata = removedMetadata
                    , normalizedSource = privateSource
                    , animation = animationHandling
                    }

newAwaitingUploadImage ::
    ImageIdentifier ->
    UploadAttemptIdentifier ->
    ImageUploadDeclaration ->
    UTCTime ->
    AwaitingUploadImage
newAwaitingUploadImage = AwaitingUploadImage

restoreAvailableImage :: ImageIdentifier -> UTCTime -> AvailableImage
restoreAvailableImage = AvailableImage

restoreRejectedImageUpload :: ImageIdentifier -> ImageRejection -> UTCTime -> RejectedImageUpload
restoreRejectedImageUpload = RejectedImageUpload

beginImageInspection ::
    UTCTime ->
    UploadAttemptIdentifier ->
    AwaitingUploadImage ->
    Either DomainError InspectingImage
beginImageInspection
    startedAt
    attemptedUpload
    (AwaitingUploadImage imageIdentifier currentUpload uploadDeclaration requestedAt)
        | attemptedUpload /= currentUpload =
            Left
                ( createOperationNotAllowed
                    "ImageInspection"
                    ( "upload attempt is not current: expected "
                        <> uploadAttemptIdentifierText currentUpload
                        <> ", actual "
                        <> uploadAttemptIdentifierText attemptedUpload
                    )
                )
        | otherwise =
            Right
                ( InspectingImage
                    imageIdentifier
                    attemptedUpload
                    uploadDeclaration
                    requestedAt
                    startedAt
                )

acceptImageInspection ::
    UTCTime -> InspectingImage -> SuccessfulImageInspection -> Either ImageRejection AvailableImage
acceptImageInspection completedAt (InspectingImage imageIdentifier _ _ _ _) inspected = do
    _ <- assessSuccessfulImageInspection inspected
    pure (AvailableImage imageIdentifier completedAt)

rejectImageInspection :: UTCTime -> ImageRejection -> InspectingImage -> RejectedImageUpload
rejectImageInspection completedAt reason (InspectingImage imageIdentifier _ _ _ _) =
    RejectedImageUpload imageIdentifier reason completedAt

restartImageUpload ::
    UploadAttemptIdentifier ->
    UTCTime ->
    Image ->
    Either DomainError AwaitingUploadImage
restartImageUpload nextAttempt requestedAt image =
    case image of
        AwaitingUpload awaiting ->
            Right
                ( foldAwaitingUploadImage
                    ( \identifier _ declaration _ ->
                        newAwaitingUploadImage
                            identifier
                            nextAttempt
                            declaration
                            requestedAt
                    )
                    awaiting
                )
        _ ->
            Left
                ( createOperationNotAllowed
                    "Image"
                    "ImageUploadCannotBeRetried"
                )

retryableInspectionAttempt :: Image -> Either DomainError UploadAttemptIdentifier
retryableInspectionAttempt image =
    case image of
        Inspecting inspecting ->
            Right (foldInspectingImage (\_ attempt _ _ _ -> attempt) inspecting)
        _ ->
            Left
                ( createOperationNotAllowed
                    "Image"
                    "ImageInspectionCannotBeRetried"
                )

foldImageUploadDeclaration ::
    (DeclaredImageContentType -> ImageByteSize -> ImageSha256 -> result) ->
    ImageUploadDeclaration ->
    result
foldImageUploadDeclaration
    transform
    (ImageUploadDeclaration declaredContentType declaredByteSize declaredSha256) =
        transform declaredContentType declaredByteSize declaredSha256

foldAwaitingUploadImage ::
    (ImageIdentifier -> UploadAttemptIdentifier -> ImageUploadDeclaration -> UTCTime -> result) ->
    AwaitingUploadImage ->
    result
foldAwaitingUploadImage
    transform
    (AwaitingUploadImage imageIdentifier attempt uploadDeclaration requestedAt) =
        transform imageIdentifier attempt uploadDeclaration requestedAt

foldInspectingImage ::
    ( ImageIdentifier ->
      UploadAttemptIdentifier ->
      ImageUploadDeclaration ->
      UTCTime ->
      UTCTime ->
      result
    ) ->
    InspectingImage ->
    result
foldInspectingImage
    transform
    (InspectingImage imageIdentifier attempt uploadDeclaration requestedAt startedAt) =
        transform imageIdentifier attempt uploadDeclaration requestedAt startedAt

foldAvailableImage :: (ImageIdentifier -> UTCTime -> result) -> AvailableImage -> result
foldAvailableImage transform (AvailableImage imageIdentifier completedAt) =
    transform imageIdentifier completedAt

foldRejectedImageUpload ::
    (ImageIdentifier -> ImageRejection -> UTCTime -> result) ->
    RejectedImageUpload ->
    result
foldRejectedImageUpload transform (RejectedImageUpload imageIdentifier reason completedAt) =
    transform imageIdentifier reason completedAt

foldSuccessfulImageInspection ::
    ( SourceImageFormat ->
      ImageDimensions ->
      ImageByteSize ->
      ImagePixelCount ->
      OrientationApplied ->
      SensitiveMetadataRemoved ->
      PrivateNormalizedSource ->
      AnimationHandling ->
      result
    ) ->
    SuccessfulImageInspection ->
    result
foldSuccessfulImageInspection
    transform
    ( SuccessfulImageInspection
            format
            dimensions
            byteSize
            pixelCount
            orientation
            metadata
            normalizedSource
            animation
        ) =
        transform
            format
            dimensions
            byteSize
            pixelCount
            orientation
            metadata
            normalizedSource
            animation

foldImageDimensions :: (ImageWidth -> ImageHeight -> result) -> ImageDimensions -> result
foldImageDimensions transform (ImageDimensions width height) = transform width height

declaredImageContentTypeText :: DeclaredImageContentType -> Text
declaredImageContentTypeText (DeclaredImageContentType value) = value

imageSha256Text :: ImageSha256 -> Text
imageSha256Text (ImageSha256 value) = value

imageByteSizeInteger :: ImageByteSize -> Integer
imageByteSizeInteger (ImageByteSize value) = value

imageWidthInteger :: ImageWidth -> Integer
imageWidthInteger (ImageWidth value) = value

imageHeightInteger :: ImageHeight -> Integer
imageHeightInteger (ImageHeight value) = value

imagePixelCountInteger :: ImagePixelCount -> Integer
imagePixelCountInteger (ImagePixelCount value) = value

positiveValueError :: Text -> DomainError
positiveValueError valueName =
    createInvariantViolation valueName "value must be greater than zero"

nonEmptyValueError :: Text -> DomainError
nonEmptyValueError valueName =
    createInvariantViolation valueName "value must not be blank"

currentImageInspectionPolicy :: ImageInspectionPolicy
currentImageInspectionPolicy =
    ImageInspectionPolicy
        (MaximumImageByteSize (ImageByteSize (20 * 1024 * 1024)))
        (MaximumImageDimensions (ImageDimensions (ImageWidth 12000) (ImageHeight 12000)))
        (MaximumImagePixelCount (ImagePixelCount 100000000))

assessSuccessfulImageInspection ::
    SuccessfulImageInspection -> Either ImageRejection SuccessfulImageInspection
assessSuccessfulImageInspection
    inspected@(SuccessfulImageInspection _ actualDimensions actualByteSize actualPixelCount _ _ _ _)
        | exceedsByteSize actualByteSize currentImageInspectionPolicy.maximumByteSize =
            Left (ImageFileTooLarge actualByteSize currentImageInspectionPolicy.maximumByteSize)
        | exceedsDimensions actualDimensions currentImageInspectionPolicy.maximumDimensions =
            Left
                ( ImageDimensionsTooLarge
                    actualDimensions
                    currentImageInspectionPolicy.maximumDimensions
                )
        | exceedsPixelCount actualPixelCount currentImageInspectionPolicy.maximumTotalPixelCount =
            Left
                ( ImageHasTooManyPixels
                    actualPixelCount
                    currentImageInspectionPolicy.maximumTotalPixelCount
                )
        | otherwise = Right inspected

exceedsByteSize :: ImageByteSize -> MaximumImageByteSize -> Bool
exceedsByteSize
    (ImageByteSize actual)
    (MaximumImageByteSize (ImageByteSize maximumValue)) =
        actual > maximumValue

exceedsDimensions :: ImageDimensions -> MaximumImageDimensions -> Bool
exceedsDimensions
    (ImageDimensions (ImageWidth actualWidth) (ImageHeight actualHeight))
    ( MaximumImageDimensions
            (ImageDimensions (ImageWidth maximumWidth) (ImageHeight maximumHeight))
        ) =
        actualWidth > maximumWidth || actualHeight > maximumHeight

exceedsPixelCount :: ImagePixelCount -> MaximumImagePixelCount -> Bool
exceedsPixelCount
    (ImagePixelCount actual)
    (MaximumImagePixelCount (ImagePixelCount maximumValue)) =
        actual > maximumValue
