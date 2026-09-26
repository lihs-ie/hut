module Media.Presentation.Handler.ReferenceProjectionQueue (
    ReferenceProjectionHandlerDependencies (..),
    mediaReferenceProjectionQueueHandler,
) where

import Cloudflare.Workers.Entrypoint.Queue
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import "media" Media.Domain.ImageUsage
import "media" Media.UseCase.ProjectImageUsage (
    ImageUsageProjectionStore,
    projectImageUsage,
 )

data ProjectionDTO = ProjectionDTO
    { eventIdentifier :: Text
    , sourcePosition :: Text
    , sourceKind :: Text
    , sourceIdentifier :: Text
    , referencedImages :: [Text]
    , occurredAt :: UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

newtype ReferenceProjectionHandlerDependencies
    = ReferenceProjectionHandlerDependencies
    { projectionStore :: ImageUsageProjectionStore
    }

mediaReferenceProjectionQueueHandler ::
    QueueConsumer ReferenceProjectionHandlerDependencies
mediaReferenceProjectionQueueHandler = handleBatch

handleBatch :: QueueConsumer ReferenceProjectionHandlerDependencies
handleBatch batch dependencies _ = do
    let store = dependencies.projectionStore
    forM_ batch.queueBatchMessages $ \message -> do
        outcome <- try @SomeException $ do
            dto <- either fail pure (eitherDecodeStrict' message.queueMessageBody)
            projection <- decodeProjection dto
            _ <- projectImageUsage store dto.eventIdentifier projection
            pure ()
        case outcome of
            Right () -> message.queueMessageAck
            Left _ -> message.queueMessageRetry (QueueRetryOptions Nothing)

decodeProjection :: ProjectionDTO -> IO ImageUsageProjection
decodeProjection dto = do
    kind <- case dto.sourceKind of
        "article" -> pure ArticleSource
        "memo" -> pure MemoSource
        "series" -> pure SeriesSource
        _ -> fail "unknown reference source kind"
    source <- either (fail . show) pure (newSourceIdentifier dto.sourceIdentifier)
    position <- either (fail . show) pure (newSourcePosition dto.sourcePosition)
    references <- traverse (either (fail . show) pure . newImageReference) dto.referencedImages
    pure (newImageUsageProjection kind source position references dto.occurredAt)
