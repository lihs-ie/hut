module Media.UseCase.ProcessImageInspectionSpec (run) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time (addUTCTime)
import Media.Domain.Image (ImageRejection (MalformedImage))
import Media.TestSupport (
    assertEqual,
    awaitingDeclaration,
    baseTime,
    successfulPngInspection,
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
    InspectionNormalization (ImageNormalized, ImagePermanentlyRejected),
    InspectionProcessingResult (InspectionCommitted, StaleUploadIgnored),
    finalObjectKeyText,
    foldProcessImageInspection,
    newFinalObjectKey,
    newProcessImageInspection,
    newTemporaryObjectKey,
    processUploadedImage,
    recordInspectionDLQFailure,
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (

 )
import Media.UseCase.RetryImageInspection qualified as RetryInspection (

 )
import Shared.UseCase.Command (Command (Command))

run :: IO Bool
run =
    and
        <$> sequence
            [ testPermanentInspectionRejection
            , testSuccessfulInspection
            , testStaleInspection
            ]

testSuccessfulInspection :: IO Bool
testSuccessfulInspection = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    evidence <- successfulPngInspection
    command <-
        testCommandAt
            baseTime
            (Process.newProcessImageInspection attempt baseTime)
    deleted <- newIORef False
    let dependencies =
            Process.InspectionDependencies
                (\_ _ _ -> pure (Just (Process.InspectionClaim awaiting temporaryKey)))
                (\_ _ _ -> pure (Process.ImageNormalized evidence))
                (pure baseTime)
                (\_ _ _ _ -> pure ())
                (\_ -> modifyIORef' deleted (const True))
                (\_ -> pure ())
        temporaryKey = Process.newTemporaryObjectKey "tmp/upload"
    result <- Process.processUploadedImage dependencies command
    wasDeleted <- readIORef deleted
    assertEqual
        "successful normalization is committed"
        True
        (case result of Process.InspectionCommitted _ -> True; _ -> False)
        <&&> assertEqual "successful temporary input is deleted" True wasDeleted

testStaleInspection :: IO Bool
testStaleInspection = do
    attempt <- testUploadAttemptIdentifier 2
    let payload = Process.newProcessImageInspection attempt baseTime
    command <-
        testCommandAt
            baseTime
            payload
    let dependencies =
            Process.InspectionDependencies
                (\_ _ _ -> pure Nothing)
                (\_ _ _ -> error "normalization must not run")
                (error "clock must not run")
                (\_ _ _ _ -> error "commit must not run")
                (\_ -> error "delete must not run")
                (\_ -> pure ())
    result <- Process.processUploadedImage dependencies command
    assertEqual
        "process payload exposes the upload fact"
        (attempt, baseTime)
        (Process.foldProcessImageInspection (,) payload)
        <&&> assertEqual
            "process payload supports equality"
            payload
            (Process.newProcessImageInspection attempt baseTime)
        <&&> assertEqual
            "process payload supports diagnostic rendering"
            True
            (not (null (show payload)))
        <&&> assertEqual
            "final object key constructor preserves text"
            "images/test"
            (Process.finalObjectKeyText (Process.newFinalObjectKey "images/test"))
        <&&> assertEqual "stale upload is ignored" Process.StaleUploadIgnored result

testPermanentInspectionRejection :: IO Bool
testPermanentInspectionRejection = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    let uploadedAt = addUTCTime (-60) baseTime
        completedAt = addUTCTime 60 baseTime
    command <-
        testCommandAt
            baseTime
            (Process.newProcessImageInspection attempt uploadedAt)
    committed <- newIORef Nothing
    claimedAt <- newIORef Nothing
    deleted <- newIORef False
    failures <- newIORef []
    let temporaryKey = Process.newTemporaryObjectKey "tmp/upload"
        dependencies =
            Process.InspectionDependencies
                ( \actualAttempt actualUploadedAt startedAt -> do
                    modifyIORef' claimedAt (const (Just (actualUploadedAt, startedAt)))
                    pure
                        ( if actualAttempt == attempt
                            then Just (Process.InspectionClaim awaiting temporaryKey)
                            else Nothing
                        )
                )
                ( \declaration _ _ ->
                    if declaration == awaitingDeclaration awaiting
                        then pure (Process.ImagePermanentlyRejected MalformedImage)
                        else error "normalizer received another declaration"
                )
                (pure completedAt)
                ( \(Command _ committedAt _ _ _) normalization result finalKey ->
                    modifyIORef'
                        committed
                        ( const
                            ( Just
                                ( normalization
                                , result
                                , Process.finalObjectKeyText finalKey
                                , committedAt
                                )
                            )
                        )
                )
                (\_ -> modifyIORef' deleted (const True))
                (\failure -> modifyIORef' failures (<> [failure]))
    processingResult <- Process.processUploadedImage dependencies command
    actualCommit <- readIORef committed
    actualClaimedAt <- readIORef claimedAt
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
                Just (Process.ImagePermanentlyRejected MalformedImage, _, key, committedAt) ->
                    key == "images/00000000000000000000000001"
                        && committedAt == completedAt
                _ -> False
            )
        <&&> assertEqual
            "R2 upload time and processing start time remain distinct"
            (Just (uploadedAt, baseTime))
            actualClaimedAt
        <&&> assertEqual "temporary input is deleted after commit" True wasDeleted
        <&&> assertEqual "DLQ failure is delegated unchanged" [failure] actualFailures
