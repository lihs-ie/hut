module Presentation.Handler.Queue.ExcerptCompletionSpec (run) where

import Control.Exception (try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Domain.Article.Common (newArticleIdentifier)
import Infrastructure.Article.Queue.ExcerptGeneration
import Presentation.Handler.Queue.ExcerptCompletion
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Domain.Error (DomainError, createProcessingTargetChanged, createServiceUnavailable)
import Shared.Domain.Excerpt (newExcerpt)
import Shared.Infrastructure.Versioning (newVersion)
import Shared.UseCase.Context (newActor, newCorrelationIdentifier)
import Shared.UseCase.Event (newEventEnvelope, newEventIdentifier)
import TestSupport (check, right, timestamp)

run :: IO ()
run = do
    identifier <- right (newEventIdentifier "completion-event")
    request <- right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    article <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    revision <- newVersion <$> (right (newPositiveInteger 3))
    excerpt <- right (newExcerpt "Generated excerpt")
    actor <- right (newActor "system")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    let message =
            ExcerptGeneratedMessage
                (newEventEnvelope identifier (timestamp 2) actor correlation Nothing
                    (ExcerptGenerated request article revision excerpt))
    applied <- newIORef False
    abandoned <- newIORef Nothing
    let dependencies =
            CompletionDependencies
                { applyGeneratedExcerpt = \_ ->
                    writeIORef applied True >> pure (Right ExcerptApplied)
                , abandonGeneration = \requestValue ->
                    writeIORef abandoned (Just requestValue) >> pure (Right ())
                }
    handleCompletionMessage dependencies message
    check "completion applies" =<< readIORef applied
    handleCompletionDeadLetterMessage dependencies message
    check "completion dead letter releases matching request"
        . (== Just (ExcerptGenerationRequested request article revision))
        =<< readIORef abandoned

    handleCompletionMessage
        dependencies{applyGeneratedExcerpt = \_ -> pure (Right ExcerptAlreadyApplied)}
        message
    handleCompletionMessage
        dependencies{applyGeneratedExcerpt = \_ -> pure (Right ExcerptNoLongerRequired)}
        message
    handleCompletionMessage
        dependencies
            { applyGeneratedExcerpt = \_ ->
                pure (Left (createProcessingTargetChanged "Article" "changed"))
            }
        message

    failed <- try
        ( handleCompletionMessage
            dependencies
                { applyGeneratedExcerpt = \_ ->
                    pure (Left (createServiceUnavailable "Article" "temporarily unavailable"))
                }
            message
        ) :: IO (Either DomainError ())
    check "temporary failure is retried" $ case failed of
        Left _ -> True
        Right _ -> False

    failedAbandon <- try
        ( handleCompletionDeadLetterMessage
            dependencies
                { abandonGeneration = \_ ->
                    pure (Left (createServiceUnavailable "Article" "temporarily unavailable"))
                }
            message
        ) :: IO (Either DomainError ())
    check "failed completion dead-letter release is retried" $ case failedAbandon of
        Left _ -> True
        Right _ -> False
