module Media.UseCase.RequestImageUploadSpec (run) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time (addUTCTime)
import Media.Domain.Image (
    Image (AwaitingUpload),
    newAwaitingUploadImage,
 )
import Media.TestSupport (
    assertEqual,
    assertLeftEqual,
    baseTime,
    destinationIdentity,
    expectRight,
    hasNoEvents,
    recordCall,
    requestDeclaration,
    testCommandAt,
    testImageIdentifier,
    testUploadAttemptIdentifier,
    validRequestPayload,
    validUploadDestinationURL,
    (<&&>),
 )
import Media.UseCase.RequestImageUpload qualified as RequestUpload (
    Dependencies (Dependencies),
    newUploadDestination,
    requestImageUpload,
    uploadDestinationURLText,
 )
import Media.UseCase.Result qualified as UseCase (
    resultEvents,
    resultOutput,
 )
import Media.UseCase.RetryImageInspection qualified as RetryInspection (

 )
import Shared.Domain.Error (createInvariantViolation)

run :: IO Bool
run =
    and
        <$> sequence
            [ testRequestImageUpload
            , testRequestImageUploadShortCircuitsIdentifierGeneration
            , testRequestImageUploadDependencyFailures
            ]

testRequestImageUpload :: IO Bool
testRequestImageUpload = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    payload <- validRequestPayload
    command <- testCommandAt baseTime payload
    calls <- newIORef []
    persisted <- newIORef Nothing
    issuedFor <- newIORef Nothing
    url <- validUploadDestinationURL
    let expiresAt = addUTCTime 900 baseTime
        expectedAwaiting =
            newAwaitingUploadImage
                imageIdentifier
                attempt
                (requestDeclaration payload)
                baseTime
        dependencies =
            RequestUpload.Dependencies
                (pure (Right imageIdentifier))
                (pure (Right attempt))
                ( \image -> do
                    modifyIORef' persisted (const (Just image))
                    recordCall calls "persist"
                )
                ( \awaiting -> do
                    modifyIORef' issuedFor (const (Just awaiting))
                    _ <- recordCall calls "destination"
                    pure
                        ( Right
                            ( RequestUpload.newUploadDestination
                                imageIdentifier
                                attempt
                                url
                                expiresAt
                            )
                        )
                )
    result <- RequestUpload.requestImageUpload dependencies command >>= expectRight "request"
    actualCalls <- readIORef calls
    actualPersisted <- readIORef persisted
    actualIssuedFor <- readIORef issuedFor
    let destination = UseCase.resultOutput result
    assertEqual "request persists before issuing destination" ["persist", "destination"] actualCalls
        <&&> assertEqual
            "request persists the complete awaiting-upload aggregate"
            ( Just
                (AwaitingUpload expectedAwaiting)
            )
            actualPersisted
        <&&> assertEqual
            "request issues the destination for the persisted aggregate"
            (Just expectedAwaiting)
            actualIssuedFor
        <&&> assertEqual
            "request returns the issued destination"
            (imageIdentifier, attempt, expiresAt)
            (destinationIdentity destination)
        <&&> assertEqual
            "upload destination URL exposes its validated text"
            "https://upload.example.test/image"
            (RequestUpload.uploadDestinationURLText url)
        <&&> assertEqual
            "request returns no events"
            True
            (hasNoEvents (UseCase.resultEvents result))

testRequestImageUploadShortCircuitsIdentifierGeneration :: IO Bool
testRequestImageUploadShortCircuitsIdentifierGeneration = do
    payload <- validRequestPayload
    command <- testCommandAt baseTime payload
    attemptCalls <- newIORef (0 :: Int)
    let expected =
            createInvariantViolation
                "ImageIdentifier"
                "identifier generation failed"
        dependencies =
            RequestUpload.Dependencies
                (pure (Left expected))
                ( do
                    modifyIORef' attemptCalls (+ 1)
                    error "upload attempt identifier must not be generated"
                )
                (\_ -> error "image must not be persisted")
                (\_ -> error "upload destination must not be issued")
    result <- RequestUpload.requestImageUpload dependencies command
    actualAttemptCalls <- readIORef attemptCalls
    assertLeftEqual "request returns the first identifier error" expected result
        <&&> assertEqual
            "request does not consume an upload attempt identifier"
            0
            actualAttemptCalls

testRequestImageUploadDependencyFailures :: IO Bool
testRequestImageUploadDependencyFailures = do
    imageIdentifier <- testImageIdentifier 1
    attempt <- testUploadAttemptIdentifier 2
    payload <- validRequestPayload
    command <- testCommandAt baseTime payload
    let attemptError = createInvariantViolation "UploadAttemptIdentifier" "generation failed"
        persistError = createInvariantViolation "Image" "persistence failed"
        destinationError = createInvariantViolation "UploadDestination" "issuance failed"
        afterAttemptMustNotRun = error "dependency after attempt generation must not run"
        afterPersistMustNotRun = error "dependency after persistence must not run"
        attemptDependencies =
            RequestUpload.Dependencies
                (pure (Right imageIdentifier))
                (pure (Left attemptError))
                afterAttemptMustNotRun
                afterAttemptMustNotRun
        persistDependencies =
            RequestUpload.Dependencies
                (pure (Right imageIdentifier))
                (pure (Right attempt))
                (\_ -> pure (Left persistError))
                afterPersistMustNotRun
        destinationDependencies =
            RequestUpload.Dependencies
                (pure (Right imageIdentifier))
                (pure (Right attempt))
                (\_ -> pure (Right ()))
                (\_ -> pure (Left destinationError))
    attemptResult <- RequestUpload.requestImageUpload attemptDependencies command
    persistResult <- RequestUpload.requestImageUpload persistDependencies command
    destinationResult <- RequestUpload.requestImageUpload destinationDependencies command
    assertLeftEqual "request stops when attempt generation fails" attemptError attemptResult
        <&&> assertLeftEqual "request stops when persistence fails" persistError persistResult
        <&&> assertLeftEqual
            "request returns destination issuance failure"
            destinationError
            destinationResult
