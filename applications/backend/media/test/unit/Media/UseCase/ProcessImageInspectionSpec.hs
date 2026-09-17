module Media.UseCase.ProcessImageInspectionSpec (run) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Media.Domain.Image (ImageRejection (MalformedImage))
import Media.TestSupport (
    assertEqual,
    baseTime,
    testAwaiting,
    testCommandAt,
    testImageIdentifier,
    testUploadAttemptIdentifier,
    (<&&>),
 )
import Media.UseCase.ProcessImageInspection qualified as Process (
    InspectionClaim (InspectionClaim),
    InspectionDependencies (InspectionDependencies),
    InspectionFailureRecord (InspectionFailureRecord),
    InspectionNormalization (ImagePermanentlyRejected),
    InspectionProcessingResult (InspectionCommitted),
    finalObjectKeyText,
    newTemporaryObjectKey,
    processUploadedImage,
    recordInspectionDLQFailure,
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (

 )
import Media.UseCase.RetryImageInspection qualified as RetryInspection (

 )

run :: IO Bool
run =
    and
        <$> sequence
            [ testPermanentInspectionRejection
            ]

testPermanentInspectionRejection :: IO Bool
testPermanentInspectionRejection = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    command <- testCommandAt baseTime attempt
    committed <- newIORef Nothing
    deleted <- newIORef False
    failures <- newIORef []
    let temporaryKey = Process.newTemporaryObjectKey "tmp/upload"
        dependencies =
            Process.InspectionDependencies
                ( \actualAttempt _ ->
                    pure
                        ( if actualAttempt == attempt
                            then Just (Process.InspectionClaim awaiting temporaryKey)
                            else Nothing
                        )
                )
                (\_ _ -> pure (Process.ImagePermanentlyRejected MalformedImage))
                ( \_ normalization result finalKey ->
                    modifyIORef'
                        committed
                        ( const
                            ( Just
                                ( normalization
                                , result
                                , Process.finalObjectKeyText finalKey
                                )
                            )
                        )
                )
                (\_ -> modifyIORef' deleted (const True))
                (\failure -> modifyIORef' failures (<> [failure]))
    processingResult <- Process.processUploadedImage dependencies command
    actualCommit <- readIORef committed
    wasDeleted <- readIORef deleted
    let failure = Process.InspectionFailureRecord attempt "queue_failed" Nothing baseTime
    Process.recordInspectionDLQFailure dependencies failure
    actualFailures <- readIORef failures
    assertEqual
        "permanent normalization rejection is committed"
        True
        (case processingResult of Process.InspectionCommitted _ -> True; _ -> False)
        <&&> assertEqual
            "rejection commit uses the final image key"
            True
            ( case actualCommit of
                Just (Process.ImagePermanentlyRejected MalformedImage, _, key) ->
                    key == "images/00000000000000000000000001"
                _ -> False
            )
        <&&> assertEqual "temporary input is deleted after commit" True wasDeleted
        <&&> assertEqual "DLQ failure is delegated unchanged" [failure] actualFailures
