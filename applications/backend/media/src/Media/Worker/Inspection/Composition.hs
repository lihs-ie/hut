module Media.Worker.Inspection.Composition (
    inspectionWorkerHandler,
) where

import Cloudflare.Workers.Binding.D1 (D1)
import Cloudflare.Workers.Binding.Images (Images)
import Cloudflare.Workers.Binding.R2 (R2Bucket)
import Cloudflare.Workers.Entrypoint.Queue (QueueConsumer)
import Cloudflare.Workers.Env (getBinding)
import Data.Proxy (Proxy (Proxy))
import Data.Time (getCurrentTime)
import Media.Infrastructure.D1.ImageRepository (
    newInspectionDependencies,
 )
import Media.Infrastructure.Images.Normalizer (
    deleteTemporaryR2Object,
    newImageNormalizer,
 )
import Media.Presentation.Handler.InspectionQueue (
    InspectionHandlerDependencies (InspectionHandlerDependencies),
    mediaInspectionQueueHandler,
 )
import Media.Worker.Inspection.Env (InspectionWorkerEnv)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Identifier (ULID, ulidText)
import Shared.FFI.SecureRandom (secureRandomBytes)
import Shared.UseCase.Context (
    CorrelationIdentifier,
    newCorrelationIdentifier,
 )
import Shared.UseCase.Identifier (
    IdentifierGenerationDependencies (IdentifierGenerationDependencies),
    generateULID,
 )

inspectionWorkerHandler :: QueueConsumer InspectionWorkerEnv
inspectionWorkerHandler batch environment context =
    mediaInspectionQueueHandler
        batch
        (inspectionHandlerDependencies environment)
        context

inspectionHandlerDependencies ::
    InspectionWorkerEnv ->
    InspectionHandlerDependencies
inspectionHandlerDependencies environment =
    InspectionHandlerDependencies
        ( newInspectionDependencies
            generateIdentifier
            database
            (newImageNormalizer temporaryBucket assetBucket images)
            (deleteTemporaryR2Object temporaryBucket)
        )
        generateCorrelationIdentifier
  where
    database =
        getBinding
            (Proxy @"MEDIA_DATABASE")
            environment ::
            D1
    temporaryBucket =
        getBinding
            (Proxy @"MEDIA_TMP_UPLOADS")
            environment ::
            R2Bucket
    assetBucket =
        getBinding
            (Proxy @"MEDIA_ASSETS")
            environment ::
            R2Bucket
    images =
        getBinding
            (Proxy @"IMAGES")
            environment ::
            Images

generateCorrelationIdentifier :: IO CorrelationIdentifier
generateCorrelationIdentifier = do
    generated <- generateIdentifier
    value <- either (fail . show) (pure . ulidText) generated
    either (fail . show) pure (newCorrelationIdentifier value)

generateIdentifier :: IO (Either DomainError ULID)
generateIdentifier =
    generateULID
        ( IdentifierGenerationDependencies
            (Right <$> getCurrentTime)
            secureRandomBytes
        )
