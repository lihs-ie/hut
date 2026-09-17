module Main (main) where

import Media.Domain.Image.RetentionSpec qualified as ImageRetentionSpec (
    run,
 )
import Media.Domain.ImageSpec qualified as ImageSpec (run)
import Media.Domain.ImageUsageSpec qualified as ImageUsageSpec (
    run,
 )
import Media.Presentation.API.RequestImageUploadSpec qualified as RequestImageUploadAPISpec (
    run,
 )
import Media.Presentation.APISpec qualified as APISpec (run)
import Media.Presentation.Handler.API.GetImageStatusSpec qualified as GetImageStatusHandlerSpec (
    run,
 )
import Media.Presentation.Handler.API.MetadataSpec qualified as MetadataSpec (
    run,
 )
import Media.Presentation.Handler.API.RequestImageUploadSpec qualified as UploadHandlerSpec (
    run,
 )
import Media.Presentation.Handler.API.RetryImageInspectionSpec qualified as RetryHandlerSpec (
    run,
 )
import Media.Presentation.Handler.API.RetryImageUploadSpec qualified as UploadRetryHandlerSpec (
    run,
 )
import Media.Presentation.Handler.InspectionQueueSpec qualified as InspectionQueueSpec (
    run,
 )
import Media.Presentation.Handler.ReferenceProjectionQueueSpec qualified as ProjectionQueueSpec (
    run,
 )
import Media.Presentation.Handler.RetentionScheduledSpec qualified as RetentionScheduledSpec (
    run,
 )
import Media.Presentation.Server.API.ServerSpec qualified as ServerSpec (
    run,
 )
import Media.UseCase.GetImageStatusSpec qualified as GetImageStatusSpec (
    run,
 )
import Media.UseCase.InspectImageSpec qualified as InspectImageSpec (
    run,
 )
import Media.UseCase.ProcessImageInspectionSpec qualified as ProcessImageInspectionSpec (
    run,
 )
import Media.UseCase.ProjectImageUsageSpec qualified as ProjectImageUsageSpec (
    run,
 )
import Media.UseCase.RequestImageUploadSpec qualified as RequestImageUploadSpec (
    run,
 )
import Media.UseCase.RetainImagesSpec qualified as RetainImagesSpec (
    run,
 )
import Media.UseCase.RetryImageInspectionSpec qualified as RetryImageInspectionSpec (
    run,
 )
import Media.UseCase.RetryImageUploadSpec qualified as RetryImageUploadSpec (
    run,
 )
import System.Exit (exitFailure)

main :: IO ()
main = do
    results <-
        sequence
            [ ImageSpec.run
            , ImageRetentionSpec.run
            , ImageUsageSpec.run
            , GetImageStatusSpec.run
            , InspectImageSpec.run
            , ProcessImageInspectionSpec.run
            , ProjectImageUsageSpec.run
            , RequestImageUploadSpec.run
            , RetainImagesSpec.run
            , RetryImageInspectionSpec.run
            , RetryImageUploadSpec.run
            , APISpec.run
            , RequestImageUploadAPISpec.run
            , GetImageStatusHandlerSpec.run
            , MetadataSpec.run
            , UploadHandlerSpec.run
            , RetryHandlerSpec.run
            , UploadRetryHandlerSpec.run
            , InspectionQueueSpec.run
            , ProjectionQueueSpec.run
            , RetentionScheduledSpec.run
            , ServerSpec.run
            ]
    if and results then pure () else exitFailure
