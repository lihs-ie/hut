module Media.Worker.Retention.Composition (
    retentionWorkerHandler,
) where

import Cloudflare.Workers.Binding.D1 (D1)
import Cloudflare.Workers.Binding.R2 (R2Bucket)
import Cloudflare.Workers.Binding.Secret (Secret)
import Cloudflare.Workers.Binding.Var (Var, unVar)
import Cloudflare.Workers.Entrypoint.Scheduled (ScheduledHandler)
import Cloudflare.Workers.Env (getBinding)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Media.Infrastructure.Cache.CloudflarePurger (
    newCloudflareCachePurger,
 )
import Media.Infrastructure.D1.ImageRepository (
    newRetentionDependencies,
 )
import Media.Infrastructure.Images.Normalizer (
    deleteRetentionR2Object,
 )
import Media.Presentation.Handler.RetentionScheduled (
    RetentionHandlerDependencies (RetentionHandlerDependencies),
    mediaRetentionScheduledHandler,
 )
import Media.Worker.Retention.Env (RetentionWorkerEnv)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Identifier (ULID)
import Shared.FFI.SecureRandom (secureRandomBytes)
import Shared.UseCase.Identifier (
    IdentifierGenerationDependencies (IdentifierGenerationDependencies),
    generateULID,
 )

retentionWorkerHandler :: ScheduledHandler RetentionWorkerEnv
retentionWorkerHandler controller environment context =
    mediaRetentionScheduledHandler
        controller
        (retentionHandlerDependencies environment)
        context

retentionHandlerDependencies ::
    RetentionWorkerEnv ->
    RetentionHandlerDependencies
retentionHandlerDependencies environment =
    RetentionHandlerDependencies
        ( newRetentionDependencies
            generateIdentifier
            database
            (deleteRetentionR2Object temporaryBucket assetBucket)
            purge
        )
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
    zone =
        getBinding
            (Proxy @"CLOUDFLARE_ZONE_IDENTIFIER")
            environment ::
            Var
    publicBaseURL =
        getBinding
            (Proxy @"MEDIA_PUBLIC_BASE_URL")
            environment ::
            Var
    token =
        getBinding
            (Proxy @"CLOUDFLARE_CACHE_PURGE_TOKEN")
            environment ::
            Secret

    purge key =
        newCloudflareCachePurger
            zone
            token
            (joinURL (unVar publicBaseURL) key)

generateIdentifier :: IO (Either DomainError ULID)
generateIdentifier =
    generateULID
        ( IdentifierGenerationDependencies
            (Right <$> getCurrentTime)
            secureRandomBytes
        )

joinURL :: Text -> Text -> Text
joinURL base path =
    Text.dropWhileEnd (== '/') base
        <> "/"
        <> Text.dropWhile (== '/') path
