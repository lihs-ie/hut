module Media.Worker.ReferenceProjection.Composition (
    referenceProjectionWorkerHandler,
) where

import Cloudflare.Workers.Binding.D1 (D1)
import Cloudflare.Workers.Entrypoint.Queue (QueueConsumer)
import Cloudflare.Workers.Env (getBinding)
import Data.Proxy (Proxy (Proxy))
import Media.Infrastructure.D1.ImageUsageRepository (
    newImageUsageProjectionStore,
 )
import Media.Presentation.Handler.ReferenceProjectionQueue (
    ReferenceProjectionHandlerDependencies (
        ReferenceProjectionHandlerDependencies
    ),
    mediaReferenceProjectionQueueHandler,
 )
import Media.Worker.ReferenceProjection.Env (
    ReferenceProjectionWorkerEnv,
 )

referenceProjectionWorkerHandler ::
    QueueConsumer ReferenceProjectionWorkerEnv
referenceProjectionWorkerHandler batch environment context =
    mediaReferenceProjectionQueueHandler
        batch
        dependencies
        context
  where
    database =
        getBinding
            (Proxy @"MEDIA_DATABASE")
            environment ::
            D1
    dependencies =
        ReferenceProjectionHandlerDependencies
            (newImageUsageProjectionStore database)
