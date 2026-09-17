module Media.UseCase.RetryImageUploadSpec (run) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time (addUTCTime)
import Media.Domain.Image (
    Image (Available, AwaitingUpload),
    imageIdentifierText,
 )
import Media.TestSupport (
    acceptedImage,
    assertEqual,
    assertLeftEqual,
    baseTime,
    expectRight,
    expectedRetriedImage,
    hasNoEvents,
    recordCall,
    testAwaiting,
    testCommandAt,
    testImageIdentifier,
    testUploadAttemptIdentifier,
    validUploadDestinationURL,
    (<&&>),
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (

 )
import Media.UseCase.Result qualified as UseCase (resultEvents)
import Media.UseCase.RetryImageInspection qualified as RetryInspection (

 )
import Media.UseCase.RetryImageUpload qualified as RetryUpload (
    Dependencies (Dependencies),
    newRetryImageUpload,
    newUploadDestination,
    retryImageUpload,
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
            [ testRetryImageUpload
            , testRetryImageUploadDependencyFailures
            ]

testRetryImageUpload :: IO Bool
testRetryImageUpload = do
    imageIdentifier <- testImageIdentifier 1
    firstAttempt <- testUploadAttemptIdentifier 2
    nextAttempt <- testUploadAttemptIdentifier 3
    original <- testAwaiting imageIdentifier firstAttempt
    let retriedAt = addUTCTime 60 baseTime
    command <- testCommandAt retriedAt (RetryUpload.newRetryImageUpload imageIdentifier)
    persistedImage <- newIORef Nothing
    calls <- newIORef []
    url <- validUploadDestinationURL
    let dependencies =
            RetryUpload.Dependencies
                (\_ -> pure (Right (Just (AwaitingUpload original))))
                (pure (Right nextAttempt))
                ( \image -> do
                    modifyIORef' persistedImage (const (Just image))
                    recordCall calls "persist"
                )
                ( \_ -> do
                    _ <- recordCall calls "destination"
                    pure
                        ( Right
                            ( RetryUpload.newUploadDestination
                                imageIdentifier
                                nextAttempt
                                url
                                (addUTCTime 900 retriedAt)
                            )
                        )
                )
    result <- RetryUpload.retryImageUpload dependencies command >>= expectRight "retry upload"
    stored <- readIORef persistedImage
    actualCalls <- readIORef calls
    assertEqual "retry persists before issuing destination" ["persist", "destination"] actualCalls
        <&&> assertEqual
            "retry preserves declaration and uses a new attempt"
            (Just (expectedRetriedImage original nextAttempt retriedAt))
            stored
        <&&> assertEqual "retry returns no events" True (hasNoEvents (UseCase.resultEvents result))

testRetryImageUploadDependencyFailures :: IO Bool
testRetryImageUploadDependencyFailures = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    nextAttempt <- testUploadAttemptIdentifier 3
    awaiting <- testAwaiting imageIdentifier attempt
    available <- acceptedImage
    command <- testCommandAt baseTime (RetryUpload.newRetryImageUpload imageIdentifier)
    let repositoryError = createInvariantViolation "Image" "lookup failed"
        attemptError = createInvariantViolation "UploadAttemptIdentifier" "generation failed"
        persistError = createInvariantViolation "Image" "persistence failed"
        destinationError = createInvariantViolation "UploadDestination" "issuance failed"
        neverAttempt = error "attempt generation must not run"
        neverPersist = error "persistence must not run"
        neverDestination = error "destination issuance must not run"
        dependencies found generated persist destination =
            RetryUpload.Dependencies found generated persist destination
    repositoryResult <-
        RetryUpload.retryImageUpload
            ( dependencies
                (\_ -> pure (Left repositoryError))
                neverAttempt
                neverPersist
                neverDestination
            )
            command
    missingResult <-
        RetryUpload.retryImageUpload
            (dependencies (\_ -> pure (Right Nothing)) neverAttempt neverPersist neverDestination)
            command
    attemptResult <-
        RetryUpload.retryImageUpload
            ( dependencies
                (\_ -> pure (Right (Just (AwaitingUpload awaiting))))
                (pure (Left attemptError))
                neverPersist
                neverDestination
            )
            command
    transitionResult <-
        RetryUpload.retryImageUpload
            ( dependencies
                (\_ -> pure (Right (Just (Available available))))
                (pure (Right nextAttempt))
                neverPersist
                neverDestination
            )
            command
    persistResult <-
        RetryUpload.retryImageUpload
            ( dependencies
                (\_ -> pure (Right (Just (AwaitingUpload awaiting))))
                (pure (Right nextAttempt))
                (\_ -> pure (Left persistError))
                neverDestination
            )
            command
    destinationResult <-
        RetryUpload.retryImageUpload
            ( dependencies
                (\_ -> pure (Right (Just (AwaitingUpload awaiting))))
                (pure (Right nextAttempt))
                (\_ -> pure (Right ()))
                (\_ -> pure (Left destinationError))
            )
            command
    let missingError = createAggregateNotFound "Image" (imageIdentifierText imageIdentifier)
        transitionError = createOperationNotAllowed "Image" "ImageUploadCannotBeRetried"
    assertLeftEqual "retry upload returns repository failure" repositoryError repositoryResult
        <&&> assertLeftEqual "retry upload reports missing image" missingError missingResult
        <&&> assertLeftEqual "retry upload stops at attempt failure" attemptError attemptResult
        <&&> assertLeftEqual
            "retry upload rejects a non-awaiting image"
            transitionError
            transitionResult
        <&&> assertLeftEqual "retry upload stops at persistence failure" persistError persistResult
        <&&> assertLeftEqual
            "retry upload returns destination failure"
            destinationError
            destinationResult
