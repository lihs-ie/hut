module Media.Presentation.Handler.InspectionQueue (
    InspectionHandlerDependencies (..),
    mediaInspectionQueueHandler,
) where

import Cloudflare.Workers.Entrypoint.Queue
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Shared.UseCase.Command (Command (Command))
import Shared.UseCase.Context (CorrelationIdentifier, newActor)
import "media" Media.Domain.Image (UploadAttemptIdentifier, uploadAttemptIdentifierFromText)
import "media" Media.UseCase.ProcessImageInspection

newtype R2ObjectData = R2ObjectData {key :: Text}
    deriving stock (Generic)
    deriving anyclass (FromJSON)

newtype R2EventNotification = R2EventNotification {data' :: R2ObjectData}
    deriving stock (Generic)

instance FromJSON R2EventNotification where
    parseJSON = withObject "R2EventNotification" $ \value ->
        R2EventNotification <$> value .: "data"

data InspectionHandlerDependencies = InspectionHandlerDependencies
    { inspection :: InspectionDependencies
    , generateCorrelationIdentifier :: IO CorrelationIdentifier
    }

mediaInspectionQueueHandler :: QueueConsumer InspectionHandlerDependencies
mediaInspectionQueueHandler = handleBatch

handleBatch :: QueueConsumer InspectionHandlerDependencies
handleBatch batch handlerDependencies _ = do
    if "dlq" `Text.isInfixOf` Text.toLower batch.queueBatchQueueName
        then forM_ batch.queueBatchMessages (handleDLQ handlerDependencies.inspection)
        else forM_ batch.queueBatchMessages (handleNormal handlerDependencies)

handleNormal :: InspectionHandlerDependencies -> QueueMessage -> IO ()
handleNormal handlerDependencies message = do
    let dependencies = handlerDependencies.inspection
    attempt <- decodeAttempt message.queueMessageBody
    correlation <- handlerDependencies.generateCorrelationIdentifier
    command <- systemCommand message correlation attempt
    outcome <- try @SomeException (processUploadedImage dependencies command)
    case outcome of
        Right _ -> message.queueMessageAck
        Left _ -> message.queueMessageRetry (QueueRetryOptions Nothing)

handleDLQ :: InspectionDependencies -> QueueMessage -> IO ()
handleDLQ dependencies message = do
    attempt <- decodeAttempt message.queueMessageBody
    recordInspectionDLQFailure
        dependencies
        InspectionFailureRecord
            { uploadAttempt = attempt
            , code = "queue_retries_exhausted"
            , detail = Nothing
            , failedAt = millisecondsToUTC message.queueMessageTimestamp
            }
    message.queueMessageAck

decodeAttempt :: ByteString -> IO UploadAttemptIdentifier
decodeAttempt bytes = do
    notification <-
        either
            fail
            pure
            (eitherDecodeStrict' bytes :: Either String R2EventNotification)
    let R2EventNotification (R2ObjectData key) = notification
        rawAttempt = last (Text.splitOn "/" key)
    either (fail . show) pure (uploadAttemptIdentifierFromText rawAttempt)

systemCommand ::
    QueueMessage ->
    CorrelationIdentifier ->
    payload ->
    IO (Command payload)
systemCommand message correlation payload = do
    actor <- either (fail . show) pure (newActor "media-inspection-worker")
    pure
        ( Command
            payload
            (millisecondsToUTC message.queueMessageTimestamp)
            actor
            correlation
            Nothing
        )

millisecondsToUTC :: Integer -> UTCTime
millisecondsToUTC value = posixSecondsToUTCTime (fromInteger value / 1000)
