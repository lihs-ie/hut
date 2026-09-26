module Media.UseCase.FindAvailableImagesSpec (run) where

import Media.TestSupport (
    assertEqual,
    assertLeftEqual,
    baseTime,
    expectRight,
    hasNoEvents,
    testCommandAt,
    testImageIdentifier,
    (<&&>),
 )
import Media.UseCase.FindAvailableImages qualified as FindAvailable (
    Dependencies (Dependencies),
    findAvailableImages,
    newFindAvailableImages,
 )
import Media.UseCase.Result qualified as UseCase (resultEvents, resultOutput)
import Shared.Domain.Error (createServiceUnavailable)

run :: IO Bool
run = and <$> sequence [findsAvailableImages, propagatesFailure, acceptsEmptyList]

findsAvailableImages :: IO Bool
findsAvailableImages = do
    available <- testImageIdentifier 1
    unavailable <- testImageIdentifier 2
    command <- testCommandAt baseTime
        (FindAvailable.newFindAvailableImages [available, unavailable])
    let dependencies = FindAvailable.Dependencies $ \identifiers ->
            pure $ if identifiers == [available, unavailable]
                then Right [available]
                else Left (createServiceUnavailable "Media" "unexpected input")
    result <- FindAvailable.findAvailableImages dependencies command
        >>= expectRight "find available images"
    assertEqual "returns available identifiers" [available] (UseCase.resultOutput result)
        <&&> assertEqual "emits no events" True
            (hasNoEvents (UseCase.resultEvents result))

propagatesFailure :: IO Bool
propagatesFailure = do
    identifier <- testImageIdentifier 1
    command <- testCommandAt baseTime
        (FindAvailable.newFindAvailableImages [identifier])
    let failure = createServiceUnavailable "Media" "database unavailable"
        dependencies = FindAvailable.Dependencies (const (pure (Left failure)))
    result <- FindAvailable.findAvailableImages dependencies command
    assertLeftEqual "propagates database failure" failure result

acceptsEmptyList :: IO Bool
acceptsEmptyList = do
    command <- testCommandAt baseTime (FindAvailable.newFindAvailableImages [])
    let dependencies = FindAvailable.Dependencies $ \identifiers ->
            pure (Right identifiers)
    result <- FindAvailable.findAvailableImages dependencies command
        >>= expectRight "empty availability"
    assertEqual "empty input returns empty output" [] (UseCase.resultOutput result)
