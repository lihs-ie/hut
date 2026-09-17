module Media.UseCase.RetainImagesSpec (run) where

import Control.Exception (SomeException, throwIO, try)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Media.Domain.Image (imageIdentifierText)
import Media.TestSupport (
    assertEqual,
    baseTime,
    testImageIdentifier,
    (<&&>),
 )
import Media.UseCase.RetainImages (
    RetentionCandidate (RetentionCandidate),
    RetentionDependencies (RetentionDependencies),
    RetentionObject (..),
    RetentionResult (deletedCount),
    retainImages,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ testRetainImagesWithNoCandidates
            , testRetainImagesDeletesTemporaryAndFinalObjects
            , testRetainImagesPropagatesDependencyFailures
            ]

testRetainImagesWithNoCandidates :: IO Bool
testRetainImagesWithNoCandidates = do
    claimedAt <- newIORef Nothing
    result <-
        retainImages
            ( RetentionDependencies
                (\now -> writeIORef claimedAt (Just now) >> pure [])
                (\_ -> error "delete must not run")
                (\_ -> error "purge must not run")
                (\_ -> error "finalize must not run")
            )
            baseTime
    actualClaimedAt <- readIORef claimedAt
    assertEqual "retention candidates are claimed at the requested time" (Just baseTime) actualClaimedAt
        <&&> assertEqual "empty retention reports zero deletions" 0 result.deletedCount

testRetainImagesDeletesTemporaryAndFinalObjects :: IO Bool
testRetainImagesDeletesTemporaryAndFinalObjects = do
    temporaryImage <- testImageIdentifier 1
    finalImage <- testImageIdentifier 2
    calls <- newIORef []
    let candidates =
            [ RetentionCandidate temporaryImage (TemporaryObject "tmp/image-1")
            , RetentionCandidate finalImage (FinalObject "images/image-2" "https://media/image-2")
            ]
        dependencies =
            RetentionDependencies
                (\_ -> pure candidates)
                (\object -> record calls ("delete:" <> objectName object))
                (\publicURL -> record calls ("purge:" <> publicURL))
                (\image -> record calls ("finalize:" <> imageIdentifierText image))
    result <- retainImages dependencies baseTime
    actualCalls <- readIORef calls
    assertEqual "all claimed candidates are reported as deleted" 2 result.deletedCount
        <&&> assertEqual
            "temporary and final objects follow their required deletion order"
            [ "delete:tmp/image-1"
            , "finalize:00000000000000000000000001"
            , "delete:images/image-2"
            , "purge:https://media/image-2"
            , "finalize:00000000000000000000000002"
            ]
            actualCalls

testRetainImagesPropagatesDependencyFailures :: IO Bool
testRetainImagesPropagatesDependencyFailures = do
    image <- testImageIdentifier 1
    let temporary = RetentionCandidate image (TemporaryObject "tmp/image-1")
        final = RetentionCandidate image (FinalObject "images/image-1" "https://media/image-1")
    claimFailure <-
        fails
            ( RetentionDependencies
                (\_ -> throwIO (userError "claim failed"))
                (\_ -> error "delete must not run")
                (\_ -> error "purge must not run")
                (\_ -> error "finalize must not run")
            )
    deleteFailure <-
        fails
            ( RetentionDependencies
                (\_ -> pure [temporary])
                (\_ -> throwIO (userError "delete failed"))
                (\_ -> error "purge must not run")
                (\_ -> error "finalize must not run")
            )
    purgeFailure <-
        fails
            ( RetentionDependencies
                (\_ -> pure [final])
                (\_ -> pure ())
                (\_ -> throwIO (userError "purge failed"))
                (\_ -> error "finalize must not run")
            )
    finalizeFailure <-
        fails
            ( RetentionDependencies
                (\_ -> pure [temporary])
                (\_ -> pure ())
                (\_ -> error "purge must not run")
                (\_ -> throwIO (userError "finalize failed"))
            )
    assertEqual
        "retention propagates claim, delete, purge, and finalize failures"
        [True, True, True, True]
        [claimFailure, deleteFailure, purgeFailure, finalizeFailure]
  where
    fails dependencies = do
        outcome <- try @SomeException (retainImages dependencies baseTime)
        pure (isFailure outcome)

record :: IORef [Text] -> Text -> IO ()
record calls value = modifyIORef' calls (<> [value])

objectName :: RetentionObject -> Text
objectName (TemporaryObject key) = key
objectName (FinalObject key _) = key

isFailure :: Either SomeException value -> Bool
isFailure (Left _) = True
isFailure (Right _) = False
