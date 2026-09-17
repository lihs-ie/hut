module Media.Worker.API.Composition (
    apiWorkerHandler,
) where

import Cloudflare.Workers.Binding.D1 (D1)
import Cloudflare.Workers.Binding.Queue (QueueProducer)
import Cloudflare.Workers.Entrypoint.Fetch (FetchHandler)
import Cloudflare.Workers.Env (getBinding)
import Data.Proxy (Proxy (Proxy))
import Data.Time (NominalDiffTime, getCurrentTime)
import Media.Infrastructure.D1.ImageRepository (
    findImageResult,
    newImageIdentifierResult,
    newUploadAttemptIdentifierResult,
    persistImageResult,
 )
import Media.Infrastructure.Queue.Inspection (newInspectionEnqueuer)
import Media.Presentation.Handler.API.GetImageStatus qualified as GetStatusHandler
import Media.Presentation.Handler.API.Metadata (
    MetadataDependencies (MetadataDependencies),
 )
import Media.Presentation.Handler.API.RequestImageUpload qualified as RequestHandler
import Media.Presentation.Handler.API.RetryImageInspection qualified as RetryInspectionHandler
import Media.Presentation.Handler.API.RetryImageUpload qualified as RetryUploadHandler
import Media.Presentation.Server.API (
    APIServerDependencies (..),
    mediaAPIServer,
 )
import Media.Worker.API.Env (APIWorkerEnv)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Identifier (ulidText)
import Shared.FFI.SecureRandom (secureRandomBytes)
import Shared.UseCase.Command (Command)
import Shared.UseCase.Identifier (
    IdentifierGenerationDependencies (IdentifierGenerationDependencies),
    generateULID,
 )
import "media" Media.Domain.Image
import "media" Media.UseCase.GetImageStatus qualified as GetImageStatus
import "media" Media.UseCase.RequestImageUpload qualified as RequestImageUpload
import "media" Media.UseCase.Result qualified as UseCase
import "media" Media.UseCase.RetryImageInspection qualified as RetryImageInspection
import "media" Media.UseCase.RetryImageUpload qualified as RetryImageUpload

type IssueUploadDestination =
    NominalDiffTime ->
    AwaitingUploadImage ->
    IO (Either DomainError RequestImageUpload.UploadDestination)

apiWorkerHandler :: IssueUploadDestination -> FetchHandler APIWorkerEnv
apiWorkerHandler issueDestination request environment context =
    mediaAPIServer
        request
        (apiServerDependencies issueDestination environment)
        context

apiServerDependencies ::
    IssueUploadDestination ->
    APIWorkerEnv ->
    APIServerDependencies
apiServerDependencies issueDestination environment =
    APIServerDependencies
        { requestImageUpload =
            RequestHandler.RequestImageUploadHandlerDependencies
                metadata
                (executeRequest requestDependencies)
        , retryImageUpload =
            RetryUploadHandler.RetryImageUploadHandlerDependencies
                metadata
                (executeRetryUpload retryDependencies)
        , getImageStatus =
            GetStatusHandler.GetImageStatusHandlerDependencies
                metadata
                (executeGetStatus getStatusDependencies)
        , retryImageInspection =
            RetryInspectionHandler.RetryImageInspectionHandlerDependencies
                metadata
                (executeRetryInspection retryInspectionDependencies)
        }
  where
    database = mediaDatabase environment
    metadata =
        MetadataDependencies
            (Right <$> getCurrentTime)
            (fmap ulidText <$> generateIdentifier)
    generateIdentifier =
        generateULID
            ( IdentifierGenerationDependencies
                (Right <$> getCurrentTime)
                secureRandomBytes
            )
    requestDependencies =
        RequestImageUpload.Dependencies
            (newImageIdentifierResult generateIdentifier)
            (newUploadAttemptIdentifierResult generateIdentifier)
            (persistImageResult database uploadDestinationLifetime)
            (issueDestination uploadDestinationLifetime)
    retryDependencies =
        RetryImageUpload.Dependencies
            (findImageResult database)
            (newUploadAttemptIdentifierResult generateIdentifier)
            (persistImageResult database uploadDestinationLifetime)
            (issueDestination uploadDestinationLifetime)
    getStatusDependencies =
        GetImageStatus.Dependencies (findImageResult database)
    retryInspectionDependencies =
        RetryImageInspection.Dependencies
            (findImageResult database)
            (newInspectionEnqueuer (inspectionQueue environment))

executeRequest ::
    RequestImageUpload.Dependencies ->
    Command RequestImageUpload.RequestImageUpload ->
    IO (Either DomainError RequestHandler.RequestImageUploadHandlerOutput)
executeRequest dependencies command =
    fmap requestOutput
        <$> RequestImageUpload.requestImageUpload dependencies command

executeRetryUpload ::
    RetryImageUpload.Dependencies ->
    Command RetryImageUpload.RetryImageUpload ->
    IO (Either DomainError RetryUploadHandler.RetryImageUploadHandlerOutput)
executeRetryUpload dependencies command =
    fmap retryUploadOutput
        <$> RetryImageUpload.retryImageUpload dependencies command

executeGetStatus ::
    GetImageStatus.Dependencies ->
    Command GetImageStatus.GetImageStatus ->
    IO (Either DomainError GetStatusHandler.GetImageStatusHandlerOutput)
executeGetStatus dependencies command =
    fmap (getStatusOutput . UseCase.resultOutput)
        <$> GetImageStatus.getImageStatus dependencies command

executeRetryInspection ::
    RetryImageInspection.Dependencies ->
    Command RetryImageInspection.RetryImageInspection ->
    IO (Either DomainError RetryInspectionHandler.RetryImageInspectionHandlerOutput)
executeRetryInspection dependencies command =
    fmap
        ( RetryInspectionHandler.RetryImageInspectionHandlerOutput
            . UseCase.resultOutput
        )
        <$> RetryImageInspection.retryImageInspection dependencies command

requestOutput ::
    RequestImageUpload.Result ->
    RequestHandler.RequestImageUploadHandlerOutput
requestOutput =
    RequestImageUpload.foldUploadDestination build . UseCase.resultOutput
  where
    build image attempt destination expiresAt =
        RequestHandler.RequestImageUploadHandlerOutput
            image
            attempt
            (RequestImageUpload.uploadDestinationURLText destination)
            expiresAt

retryUploadOutput ::
    RetryImageUpload.Result ->
    RetryUploadHandler.RetryImageUploadHandlerOutput
retryUploadOutput =
    RetryImageUpload.foldUploadDestination build . UseCase.resultOutput
  where
    build image attempt destination expiresAt =
        RetryUploadHandler.RetryImageUploadHandlerOutput
            image
            attempt
            (RetryImageUpload.uploadDestinationURLText destination)
            expiresAt

getStatusOutput :: Image -> GetStatusHandler.GetImageStatusHandlerOutput
getStatusOutput image =
    case image of
        AwaitingUpload awaiting ->
            foldAwaitingUploadImage
                (\identifier _ _ _ -> status identifier "awaiting_upload")
                awaiting
        Inspecting inspecting ->
            foldInspectingImage
                (\identifier _ _ _ _ -> status identifier "inspecting")
                inspecting
        Available available ->
            foldAvailableImage
                (\identifier _ -> status identifier "available")
                available
        Rejected rejected ->
            foldRejectedImageUpload
                (\identifier _ _ -> status identifier "rejected")
                rejected
  where
    status = GetStatusHandler.GetImageStatusHandlerOutput

mediaDatabase :: APIWorkerEnv -> D1
mediaDatabase = getBinding (Proxy @"MEDIA_DATABASE")

inspectionQueue :: APIWorkerEnv -> QueueProducer
inspectionQueue = getBinding (Proxy @"MEDIA_INSPECTION_QUEUE")

uploadDestinationLifetime :: NominalDiffTime
uploadDestinationLifetime = 900
