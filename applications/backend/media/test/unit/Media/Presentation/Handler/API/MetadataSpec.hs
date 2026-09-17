module Media.Presentation.Handler.API.MetadataSpec (run) where

import Media.Presentation.Handler.API.Metadata (
    MetadataDependencies (..),
    canonicalCorrelation,
 )
import Media.Presentation.Handler.API.TestSupport (
    actorValue,
    correlated,
    emergency,
    generated,
    generatedBytes,
    isError,
    metadataDependencies,
    named,
    notFoundError,
    runHandler,
    supplied,
    unavailableError,
    uploadResponse,
    uploadWith,
 )

run :: IO Bool
run =
    and
        <$> sequence
            [ named "missing correlation is generated" missingCorrelation
            , named "missing actor is rejected" missingActor
            , named "invalid correlation is rejected" invalidCorrelation
            , named "correlation source failure" correlationSourceFailure
            , named "malformed generated correlation" malformedGeneratedCorrelation
            , named "timestamp source failure" timestampSourceFailure
            , named "invalid actor" invalidActor
            , named "canonical correlation rules" canonicalCorrelationRules
            ]

missingCorrelation :: IO Bool
missingCorrelation = do
    result <- runHandler $ uploadWith metadataDependencies (Just actorValue) Nothing
    pure $ correlated result uploadResponse generatedBytes

invalidCorrelation :: IO Bool
invalidCorrelation = do
    result <- runHandler $ uploadWith metadataDependencies (Just actorValue) (Just "bad")
    pure $ isError 400 "invalid_correlation_identifier" generated result

correlationSourceFailure :: IO Bool
correlationSourceFailure = do
    let metadata = metadataDependencies{generateCorrelationIdentifier = pure (Left notFoundError)}
    result <- runHandler $ uploadWith metadata (Just actorValue) Nothing
    pure $ isError 500 "correlation_generation_failed" emergency result

malformedGeneratedCorrelation :: IO Bool
malformedGeneratedCorrelation = do
    let metadata = metadataDependencies{generateCorrelationIdentifier = pure (Right "bad")}
    result <- runHandler $ uploadWith metadata (Just actorValue) Nothing
    pure $ isError 500 "correlation_generation_failed" emergency result

timestampSourceFailure :: IO Bool
timestampSourceFailure = do
    let metadata = metadataDependencies{currentTime = pure (Left unavailableError)}
    result <- runHandler $ uploadWith metadata (Just actorValue) (Just supplied)
    pure $ isError 500 "timestamp_generation_failed" supplied result

invalidActor :: IO Bool
invalidActor = do
    result <-
        runHandler
            (uploadWith metadataDependencies (Just "") Nothing)
    pure $ isError 400 "invalid_actor" generated result

missingActor :: IO Bool
missingActor = do
    result <- runHandler $ uploadWith metadataDependencies Nothing Nothing
    pure $ isError 400 "invalid_actor" generated result

canonicalCorrelationRules :: IO Bool
canonicalCorrelationRules =
    pure $
        canonicalCorrelation supplied == Just supplied
            && canonicalCorrelation "01k00000000000000000000003" == Nothing
            && canonicalCorrelation "bad" == Nothing
