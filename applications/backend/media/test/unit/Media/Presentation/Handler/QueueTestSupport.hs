module Media.Presentation.Handler.QueueTestSupport where

import Cloudflare.Workers.Entrypoint.Queue (
    QueueBatch (..),
    QueueMessage (..),
    QueueRetryOptions (..),
 )
import Cloudflare.Workers.Entrypoint.Scheduled (
    ScheduledController (..),
 )
import Cloudflare.Workers.HostTestKit (phantomJSVal)
import Cloudflare.Workers.Reactor (
    WorkersExecutionContext (WorkersExecutionContext),
 )
import Control.Exception (SomeException, try)
import Data.Aeson (ToJSON, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString (
    toStrict,
 )
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text (pack)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Media.Presentation.Handler.InspectionQueue (
    InspectionHandlerDependencies (InspectionHandlerDependencies),
    mediaInspectionQueueHandler,
 )
import Shared.Domain.Identifier (ulidFromInteger)
import Shared.UseCase.Command (
    Command (..),
    actorText,
    correlationIdentifierText,
 )
import Shared.UseCase.Context (CorrelationIdentifier, newCorrelationIdentifier)
import "media" Media.Domain.Image (
    AwaitingUploadImage,
    ImageByteSize,
    ImageIdentifier,
    ImagePixelCount,
    SourceImageFormat (SourcePNG),
    SuccessfulImageInspection,
    UploadAttemptIdentifier,
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
    uploadAttemptIdentifierText,
 )
import "media" Media.UseCase.ProcessImageInspection (
    InspectionDependencies (InspectionDependencies),
    TemporaryObjectKey,
    newTemporaryObjectKey,
 )
import "media" Media.UseCase.RetainImages (
    RetentionObject (..),
 )

data Disposition = Disposition
    { acknowledgements :: IORef Int
    , retryOptions :: IORef [QueueRetryOptions]
    }

named :: String -> IO Bool -> IO Bool
named label test = do
    outcome <- try @SomeException test
    case outcome of
        Right True -> putStrLn ("PASS: " <> label) >> pure True
        Right False -> putStrLn ("FAILED: " <> label) >> pure False
        Left exception -> do
            putStrLn ("FAILED: " <> label <> ": " <> show exception)
            pure False

newDisposition :: IO Disposition
newDisposition = Disposition <$> newIORef 0 <*> newIORef []

readDisposition :: Disposition -> IO (Int, [QueueRetryOptions])
readDisposition disposition =
    (,) <$> readIORef disposition.acknowledgements <*> readIORef disposition.retryOptions

queueMessage :: Disposition -> Text -> ByteString -> QueueMessage
queueMessage disposition messageID body =
    QueueMessage
        messageID
        fixedTimeMilliseconds
        1
        body
        (modifyIORef' disposition.acknowledgements (+ 1))
        (\options -> modifyIORef' disposition.retryOptions (<> [options]))

queueMessageForBody :: Text -> Disposition -> ByteString -> QueueMessage
queueMessageForBody messageID disposition body = queueMessage disposition messageID body

queueBatch :: Text -> [QueueMessage] -> QueueBatch
queueBatch name messages =
    QueueBatch name messages Nothing (pure ()) (const (pure ()))

runInspection ::
    Text ->
    InspectionDependencies ->
    Disposition ->
    ByteString ->
    Text ->
    IO ()
runInspection queueName dependencies disposition body messageID =
    mediaInspectionQueueHandler
        (queueBatch queueName [queueMessage disposition messageID body])
        (InspectionHandlerDependencies dependencies (pure fixedCorrelation))
        executionContext

fixedCorrelation :: CorrelationIdentifier
fixedCorrelation =
    either (error . show) id (newCorrelationIdentifier validMessageID)

emptyInspectionDependencies :: InspectionDependencies
emptyInspectionDependencies =
    InspectionDependencies
        (\_ _ -> pure Nothing)
        (\_ _ -> error "normalization must not run")
        (\_ _ _ _ -> error "commit must not run")
        (\_ -> error "temporary deletion must not run")
        (\_ -> error "DLQ persistence must not run")

commandMetadataMatches ::
    UploadAttemptIdentifier ->
    Maybe (Command UploadAttemptIdentifier) ->
    Bool
commandMetadataMatches expectedAttempt command =
    case command of
        Just actual ->
            actual.payload == expectedAttempt
                && actual.timestamp == fixedTime
                && actorText actual.actor == "media-inspection-worker"
                && correlationIdentifierText actual.correlation == validMessageID
                && actual.causation == Nothing
        Nothing -> False

inspectionBody :: UploadAttemptIdentifier -> ByteString
inspectionBody attempt =
    r2Body ("tmp/uploads/" <> uploadAttemptIdentifierText attempt)

r2Body :: Text -> ByteString
r2Body key = strictEncode (object ["data" .= object ["key" .= key]])

cloudflareR2Body :: Text -> ByteString
cloudflareR2Body key =
    strictEncode
        ( object
            [ "account" .= ("account" :: Text)
            , "action" .= ("PutObject" :: Text)
            , "bucket" .= ("hut-media-tmp-uploads-dev" :: Text)
            , "object"
                .= object
                    [ "key" .= key
                    , "size" .= (2048 :: Integer)
                    , "eTag" .= ("etag" :: Text)
                    ]
            , "eventTime" .= fixedTime
            ]
        )

projectionBody :: Text -> [Text] -> ByteString
projectionBody kind references =
    projectionBodyWith kind "source-1" "position-1" references

projectionBodyWith :: Text -> Text -> Text -> [Text] -> ByteString
projectionBodyWith kind source position references =
    strictEncode
        ( object
            [ "eventIdentifier" .= ("event-" <> kind)
            , "sourcePosition" .= position
            , "sourceKind" .= kind
            , "sourceIdentifier" .= source
            , "referencedImages" .= references
            , "occurredAt" .= fixedTime
            ]
        )

strictEncode :: (ToJSON value) => value -> ByteString
strictEncode = LazyByteString.toStrict . encode

testAwaitingImage :: UploadAttemptIdentifier -> IO AwaitingUploadImage
testAwaitingImage attempt = do
    identifier <- testImageIdentifier 1
    contentType <- expectRight (newDeclaredImageContentType "image/png")
    byteSize <- validByteSize
    sha256 <-
        expectRight
            ( newImageSha256
                "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            )
    pure
        ( newAwaitingUploadImage
            identifier
            attempt
            (newImageUploadDeclaration contentType byteSize sha256)
            fixedTime
        )

testInspectionEvidence :: IO SuccessfulImageInspection
testInspectionEvidence = do
    width <- expectRight (newImageWidth 1200)
    height <- expectRight (newImageHeight 800)
    byteSize <- validByteSize
    pixelCount <- validPixelCount
    expectRight
        ( newSuccessfulImageInspection
            SourcePNG
            (newImageDimensions width height)
            byteSize
            pixelCount
            orientationApplied
            sensitiveMetadataRemoved
            privateNormalizedSource
            imageWithoutAnimation
        )

validByteSize :: IO ImageByteSize
validByteSize = expectRight (newImageByteSize 2048)

validPixelCount :: IO ImagePixelCount
validPixelCount = expectRight (newImagePixelCount 960000)

testImageIdentifier :: Integer -> IO ImageIdentifier
testImageIdentifier value = newImageIdentifier <$> expectRight (ulidFromInteger value)

testUploadAttemptIdentifier :: Integer -> IO UploadAttemptIdentifier
testUploadAttemptIdentifier value =
    newUploadAttemptIdentifier <$> expectRight (ulidFromInteger value)

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight (Right value) = pure value
expectRight (Left err) = fail (show err)

temporaryKey :: TemporaryObjectKey
temporaryKey = newTemporaryObjectKey "tmp/upload"

scheduledController :: ScheduledController
scheduledController =
    ScheduledController "0 0 * * *" fixedTimeMilliseconds (pure ())

executionContext :: WorkersExecutionContext
executionContext = WorkersExecutionContext phantomJSVal

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 9 12) 0

fixedTimeMilliseconds :: Integer
fixedTimeMilliseconds = round (utcTimeToPOSIXSeconds fixedTime * 1000)

validMessageID :: Text
validMessageID = "00000000000000000000000003"

objectName :: RetentionObject -> Text
objectName (TemporaryObject key) = key
objectName (FinalObject key _) = key

showText :: (Show value) => value -> Text
showText = Text.pack . show

isLeft :: Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
