module Media.UseCase.ProjectImageUsageSpec (run) where

import Control.Exception (SomeException, throwIO, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Media.Domain.ImageUsage qualified as Usage (
    ImageUsageProjection,
    SourceKind (ArticleSource),
    newImageReference,
    newImageUsageProjection,
    newSourceIdentifier,
    newSourcePosition,
 )
import Media.TestSupport (
    assertEqual,
    baseTime,
    expectRight,
    (<&&>),
 )
import Media.UseCase.ProjectImageUsage (
    ImageUsageProjectionStore (ImageUsageProjectionStore),
    ProjectionApplyResult (..),
    projectImageUsage,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ testProjectImageUsageDelegatesStoreOutcomes
            , testProjectImageUsagePropagatesStoreFailure
            ]

testProjectImageUsageDelegatesStoreOutcomes :: IO Bool
testProjectImageUsageDelegatesStoreOutcomes = do
    projection <- testProjection
    captured <- newIORef Nothing
    let eventIdentifier = "event-1"
        outcomes =
            [ ProjectionApplied
            , ProjectionDuplicate
            , ProjectionOutOfOrder
            ]
        apply outcome =
            projectImageUsage
                ( ImageUsageProjectionStore $ \event actualProjection -> do
                    writeIORef captured (Just (event, actualProjection))
                    pure outcome
                )
                eventIdentifier
                projection
    actualOutcomes <- traverse apply outcomes
    actualInput <- readIORef captured
    assertEqual "projection store receives the public input" (Just (eventIdentifier, projection)) actualInput
        <&&> assertEqual "projection store outcomes are returned unchanged" outcomes actualOutcomes

testProjectImageUsagePropagatesStoreFailure :: IO Bool
testProjectImageUsagePropagatesStoreFailure = do
    projection <- testProjection
    outcome <-
        try @SomeException
            ( projectImageUsage
                (ImageUsageProjectionStore (\_ _ -> throwIO (userError "projection store unavailable")))
                "event-1"
                projection
            )
    assertEqual "projection store failure is propagated" True (isFailure outcome)

testProjection :: IO Usage.ImageUsageProjection
testProjection = do
    source <- expectRight "source fixture" (Usage.newSourceIdentifier "article-1")
    position <- expectRight "position fixture" (Usage.newSourcePosition "body")
    reference <- expectRight "image reference fixture" (Usage.newImageReference "image-1")
    pure
        ( Usage.newImageUsageProjection
            Usage.ArticleSource
            source
            position
            [reference]
            baseTime
        )

isFailure :: Either SomeException value -> Bool
isFailure (Left _) = True
isFailure (Right _) = False
