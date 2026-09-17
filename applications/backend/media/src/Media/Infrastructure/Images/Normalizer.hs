module Media.Infrastructure.Images.Normalizer (
    newImageNormalizer,
    deleteTemporaryR2Object,
    deleteRetentionR2Object,
) where

import Cloudflare.Workers.Binding.Images
import Cloudflare.Workers.Binding.R2
import Cloudflare.Workers.Streaming (
    ReadableStreamReadError,
    readableStreamToLazyByteString,
 )
import Control.Exception (SomeException, fromException, throwIO, try)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Shared.Domain.Error (
    createServiceUnavailable,
    createUnexpectedError,
 )
import System.IO (hPutStrLn, stderr)
import "media" Media.Domain.Image
import "media" Media.UseCase.ProcessImageInspection (
    FinalObjectKey,
    InspectionNormalization (ImageNormalized, ImagePermanentlyRejected),
    TemporaryObjectKey,
    finalObjectKeyText,
    temporaryObjectKeyText,
 )
import "media" Media.UseCase.RetainImages (RetentionObject (..))

newImageNormalizer ::
    R2Bucket ->
    R2Bucket ->
    Images ->
    TemporaryObjectKey ->
    FinalObjectKey ->
    IO InspectionNormalization
newImageNormalizer temporaryBucket assetBucket images temporaryKey finalKey =
    infrastructureOperation "normalize image" $ do
        source <- requireObject temporaryBucket (temporaryObjectKeyText temporaryKey)
        inspected <- imagesInfo images source.r2ObjectBody
        case inspected of
            Left err
                | permanentImagesError err ->
                    pure (ImagePermanentlyRejected MalformedImage)
                | otherwise ->
                    throwIO
                        ( createServiceUnavailable
                            "CloudflareImages"
                            "image inspection failed"
                        )
            Right SVGImageInfo ->
                pure (ImagePermanentlyRejected UnsupportedImageFormat)
            Right (RasterImageInfo format sourceBytes width height) ->
                case outputFor format of
                    Nothing ->
                        pure (ImagePermanentlyRejected UnsupportedImageFormat)
                    Just output ->
                        normalizeRaster format sourceBytes width height output
  where
    normalizeRaster format sourceBytes width height output = do
        freshSource <- requireObject temporaryBucket (temporaryObjectKeyText temporaryKey)
        transformed <- imagesTransform images freshSource.r2ObjectBody [] output
        normalized <-
            either
                ( const
                    ( throwIO
                        ( createServiceUnavailable
                            "CloudflareImages"
                            "image transformation failed"
                        )
                    )
                )
                pure
                transformed
        normalizedBytes <-
            readableStreamToLazyByteString
                maximumNormalizedByteSize
                normalized.imageOutputBody
                >>= either normalizedStreamFailure pure
        let metadata =
                r2HttpMetadataDefault
                    { r2HttpMetadataContentType = Just normalized.imageOutputContentType
                    , r2HttpMetadataCacheControl = Just "public, max-age=31536000, immutable"
                    }
            options =
                r2PutDefaultOptions
                    { r2ExtendedPutHttpMetadata = metadata
                    , r2ExtendedPutOnlyIf =
                        Just
                            ( R2OnlyIfConditional
                                (R2Condition Nothing (Just "*") Nothing Nothing)
                            )
                    }
        stored <-
            r2Put
                assetBucket
                (finalObjectKeyText finalKey)
                (R2PutBytes (LazyByteString.toStrict normalizedBytes))
                options
        case stored of
            R2PutPreconditionFailed ->
                throwIO
                    ( createUnexpectedError
                        "MediaAssetBucket"
                        "immutable final image object already exists"
                    )
            R2PutStored _ -> pure ()
        evidence <- newEvidence format sourceBytes width height output
        pure (ImageNormalized evidence)

    normalizedStreamFailure :: ReadableStreamReadError -> IO value
    normalizedStreamFailure failure =
        throwIO
            ( createUnexpectedError
                "CloudflareImages"
                ("could not buffer normalized image: " <> Text.pack (show failure))
            )

maximumNormalizedByteSize :: Int
maximumNormalizedByteSize = 20 * 1024 * 1024

newEvidence ::
    ImageFormat ->
    Integer ->
    ImageDimension ->
    ImageDimension ->
    ImageOutputOptions ->
    IO SuccessfulImageInspection
newEvidence format sourceBytes width height output = do
    domainWidth <- either invalid pure (newImageWidth (toInteger (imageDimensionPixels width)))
    domainHeight <- either invalid pure (newImageHeight (toInteger (imageDimensionPixels height)))
    byteSize <- either invalid pure (newImageByteSize sourceBytes)
    pixelCount <-
        either
            invalid
            pure
            ( newImagePixelCount
                (toInteger (imageDimensionPixels width) * toInteger (imageDimensionPixels height))
            )
    either
        ( const
            ( throwIO
                ( createUnexpectedError
                    "ImageInspectionEvidence"
                    "Cloudflare Images returned invalid evidence"
                )
            )
        )
        pure
        ( newSuccessfulImageInspection
            (sourceFormat format)
            (newImageDimensions domainWidth domainHeight)
            byteSize
            pixelCount
            orientationApplied
            sensitiveMetadataRemoved
            privateNormalizedSource
            (animationProof output)
        )
  where
    invalid _ =
        throwIO
            ( createUnexpectedError
                "ImageInspectionEvidence"
                "Cloudflare Images returned an invalid dimension or byte size"
            )

outputFor :: ImageFormat -> Maybe ImageOutputOptions
outputFor ImagePNG = Just (ImageOutputOptions OutputPNG FirstFrameOnly)
outputFor ImageJPEG = Just (ImageOutputOptions OutputWebP FirstFrameOnly)
outputFor ImageWebP = Just (ImageOutputOptions OutputWebP FirstFrameOnly)
outputFor ImageHEIC = Just (ImageOutputOptions OutputWebP FirstFrameOnly)
outputFor ImageGIF = Just (ImageOutputOptions OutputGIF PreserveAnimation)
outputFor _ = Nothing

sourceFormat :: ImageFormat -> SourceImageFormat
sourceFormat ImagePNG = SourcePNG
sourceFormat ImageJPEG = SourceJPEG
sourceFormat ImageWebP = SourceWebP
sourceFormat ImageGIF = SourceGIF
sourceFormat ImageHEIC = SourceHEIC
sourceFormat _ = error "unsupported image format"

animationProof :: ImageOutputOptions -> AnimationHandling
animationProof (ImageOutputOptions OutputGIF PreserveAnimation) = gifAnimationPreserved
animationProof _ = imageWithoutAnimation

requireObject :: R2Bucket -> Text -> IO R2Object
requireObject bucket key = do
    result <- r2Get bucket key r2GetDefaultOptions
    case result of
        R2GetSuccess object -> pure object
        R2GetNotFound ->
            throwIO
                ( createServiceUnavailable
                    "MediaTemporaryBucket"
                    "temporary upload object was not found"
                )
        R2GetPreconditionFailed _ ->
            throwIO
                ( createServiceUnavailable
                    "MediaTemporaryBucket"
                    "temporary upload precondition failed"
                )

deleteTemporaryR2Object :: R2Bucket -> TemporaryObjectKey -> IO ()
deleteTemporaryR2Object bucket key =
    infrastructureOperation
        "delete temporary image object"
        (r2Delete bucket (temporaryObjectKeyText key))

deleteRetentionR2Object :: R2Bucket -> R2Bucket -> RetentionObject -> IO ()
deleteRetentionR2Object temporaryBucket _ (TemporaryObject key) =
    infrastructureOperation
        "delete retained temporary image object"
        (r2Delete temporaryBucket key)
deleteRetentionR2Object _ assetBucket (FinalObject key _) =
    infrastructureOperation
        "delete retained final image object"
        (r2Delete assetBucket key)

infrastructureOperation :: Text -> IO value -> IO value
infrastructureOperation operation action = do
    outcome <- try @SomeException action
    case outcome of
        Right value -> pure value
        Left exception -> do
            hPutStrLn
                stderr
                ("media image infrastructure failed: " <> show exception)
            throwIO (toDomainError exception)
  where
    toDomainError exception =
        maybe
            ( createServiceUnavailable
                "MediaImageInfrastructure"
                (operation <> " failed")
            )
            id
            (fromException exception)

permanentImagesError :: ImagesError -> Bool
permanentImagesError errorValue =
    any
        (`Text.isInfixOf` Text.toLower errorValue.imageErrorMessage)
        ["malformed", "invalid image", "unsupported format"]
