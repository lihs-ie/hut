module Media.Presentation.Handler.RetentionScheduledSpec (run) where

import Control.Exception (SomeException, throwIO, try)
import Data.IORef (
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
 )
import Media.Presentation.Handler.QueueTestSupport (
    executionContext,
    fixedTime,
    isLeft,
    named,
    objectName,
    scheduledController,
    testImageIdentifier,
 )
import Media.Presentation.Handler.RetentionScheduled (
    RetentionHandlerDependencies (RetentionHandlerDependencies),
    mediaRetentionScheduledHandler,
 )
import "media" Media.Domain.Image (
    imageIdentifierText,
 )
import "media" Media.UseCase.ProcessImageInspection (

 )
import "media" Media.UseCase.RetainImages (
    RetentionCandidate (RetentionCandidate),
    RetentionDependencies (RetentionDependencies),
    RetentionObject (..),
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ named "retention success" retentionSuccess
            , named "retention failure raises" retentionFailure
            ]

retentionSuccess :: IO Bool
retentionSuccess = do
    firstImage <- testImageIdentifier 1
    secondImage <- testImageIdentifier 2
    claimedAt <- newIORef Nothing
    calls <- newIORef []
    let candidates =
            [ RetentionCandidate firstImage (TemporaryObject "tmp/a")
            , RetentionCandidate secondImage (FinalObject "images/b" "https://media/b")
            ]
        dependencies =
            RetentionDependencies
                (\now -> writeIORef claimedAt (Just now) >> pure candidates)
                ( \retentionObject ->
                    modifyIORef' calls (<> ["delete:" <> objectName retentionObject])
                )
                (\url -> modifyIORef' calls (<> ["purge:" <> url]))
                ( \image ->
                    modifyIORef' calls (<> ["finalize:" <> imageIdentifierText image])
                )
    mediaRetentionScheduledHandler
        scheduledController
        (RetentionHandlerDependencies dependencies)
        executionContext
    actualTime <- readIORef claimedAt
    actualCalls <- readIORef calls
    pure
        ( actualTime == Just fixedTime
            && actualCalls
                == [ "delete:tmp/a"
                   , "finalize:00000000000000000000000001"
                   , "delete:images/b"
                   , "purge:https://media/b"
                   , "finalize:00000000000000000000000002"
                   ]
        )

retentionFailure :: IO Bool
retentionFailure = do
    let dependencies =
            RetentionDependencies
                (\_ -> throwIO (userError "D1 unavailable"))
                (\_ -> error "delete must not run")
                (\_ -> error "purge must not run")
                (\_ -> error "finalize must not run")
    outcome <-
        try @SomeException
            ( mediaRetentionScheduledHandler
                scheduledController
                (RetentionHandlerDependencies dependencies)
                executionContext
            )
    pure (isLeft outcome)
