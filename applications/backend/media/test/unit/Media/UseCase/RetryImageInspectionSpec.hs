module Media.UseCase.RetryImageInspectionSpec (run) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time (addUTCTime)
import Media.Domain.Image (
    Image (AwaitingUpload, Inspecting),
    beginImageInspection,
    imageIdentifierText,
 )
import Media.TestSupport (
    assertEqual,
    assertLeftEqual,
    baseTime,
    expectRight,
    hasNoEvents,
    testAwaiting,
    testCommandAt,
    testImageIdentifier,
    testUploadAttemptIdentifier,
    (<&&>),
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (

 )
import Media.UseCase.Result qualified as UseCase (
    resultEvents,
    resultOutput,
 )
import Media.UseCase.RetryImageInspection qualified as RetryInspection (
    Dependencies (Dependencies),
    newRetryImageInspection,
    retryImageInspection,
 )
import Shared.Domain.Error (
    createAggregateNotFound,
    createInvariantViolation,
    createOperationNotAllowed,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ testRetryImageInspection
            , testRetryImageInspectionRejectsNonInspecting
            , testRetryImageInspectionDependencyFailures
            ]

testRetryImageInspection :: IO Bool
testRetryImageInspection = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    inspecting <-
        expectRight
            "inspection fixture"
            (beginImageInspection (addUTCTime 30 baseTime) attempt awaiting)
    command <- testCommandAt baseTime (RetryInspection.newRetryImageInspection imageIdentifier)
    enqueued <- newIORef Nothing
    let dependencies =
            RetryInspection.Dependencies
                (\_ -> pure (Right (Just (Inspecting inspecting))))
                ( \actual -> do
                    modifyIORef' enqueued (const (Just actual))
                    pure (Right ())
                )
    result <-
        RetryInspection.retryImageInspection dependencies command
            >>= expectRight "retry inspection"
    actualAttempt <- readIORef enqueued
    assertEqual "retry enqueues the current attempt" (Just attempt) actualAttempt
        <&&> assertEqual "retry returns the current attempt" attempt (UseCase.resultOutput result)
        <&&> assertEqual "retry returns no events" True (hasNoEvents (UseCase.resultEvents result))

testRetryImageInspectionRejectsNonInspecting :: IO Bool
testRetryImageInspectionRejectsNonInspecting = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    command <- testCommandAt baseTime (RetryInspection.newRetryImageInspection imageIdentifier)
    let dependencies =
            RetryInspection.Dependencies
                (\_ -> pure (Right (Just (AwaitingUpload awaiting))))
                (\_ -> pure (Right ()))
        expected =
            createOperationNotAllowed
                "Image"
                "ImageInspectionCannotBeRetried"
    result <- RetryInspection.retryImageInspection dependencies command
    assertLeftEqual "only Inspecting can retry inspection" expected result

testRetryImageInspectionDependencyFailures :: IO Bool
testRetryImageInspectionDependencyFailures = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    awaiting <- testAwaiting imageIdentifier attempt
    inspecting <- expectRight "inspection fixture" (beginImageInspection baseTime attempt awaiting)
    command <- testCommandAt baseTime (RetryInspection.newRetryImageInspection imageIdentifier)
    let repositoryError = createInvariantViolation "Image" "lookup failed"
        enqueueError = createInvariantViolation "ImageInspection" "enqueue failed"
        neverEnqueue = error "enqueue must not run"
    repositoryResult <-
        RetryInspection.retryImageInspection
            (RetryInspection.Dependencies (\_ -> pure (Left repositoryError)) neverEnqueue)
            command
    missingResult <-
        RetryInspection.retryImageInspection
            (RetryInspection.Dependencies (\_ -> pure (Right Nothing)) neverEnqueue)
            command
    enqueueResult <-
        RetryInspection.retryImageInspection
            ( RetryInspection.Dependencies
                (\_ -> pure (Right (Just (Inspecting inspecting))))
                (\_ -> pure (Left enqueueError))
            )
            command
    let missingError = createAggregateNotFound "Image" (imageIdentifierText imageIdentifier)
    assertLeftEqual
        "retry inspection returns repository failure"
        repositoryError
        repositoryResult
        <&&> assertLeftEqual "retry inspection reports missing image" missingError missingResult
        <&&> assertLeftEqual "retry inspection returns enqueue failure" enqueueError enqueueResult
