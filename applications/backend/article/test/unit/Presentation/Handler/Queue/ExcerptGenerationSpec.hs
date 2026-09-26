module Presentation.Handler.Queue.ExcerptGenerationSpec (run) where

import Control.Exception (try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Domain.Article.Common (newArticleIdentifier)
import Domain.Article.Draft (proofread, proofreadedContent)
import Infrastructure.Article.Queue.ExcerptGeneration
import Presentation.Handler.Queue.ExcerptGeneration
import Shared.Domain.Common.Primitive (newPositiveInteger)
import Shared.Domain.Error (DomainError, createProcessingTargetChanged, createServiceUnavailable)
import Shared.Domain.Excerpt (newExcerpt)
import Shared.Infrastructure.Versioning (newVersion)
import Shared.UseCase.Context (newActor, newCorrelationIdentifier)
import Shared.UseCase.Event (EventEnvelope (..), newEventEnvelope, newEventIdentifier)
import TestSupport (check, confirmed, right, start, timestamp)

run :: IO ()
run = do
    source <- right (newEventIdentifier "source-event")
    generatedIdentifier <- right (newEventIdentifier "generated-event")
    requestIdentifier <- right (newGenerationRequestIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAV")
    articleIdentifier <- right (newArticleIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAW")
    revision <- newVersion <$> (right (newPositiveInteger 3))
    actor <- right (newActor "admin")
    correlation <- right (newCorrelationIdentifier "01ARZ3NDEKTSV4RRFFQ69G5FAX")
    excerpt <- right (newExcerpt "Article excerpt")
    draft <- right start
    available <- right confirmed
    proofreaded <- right (proofread (timestamp 1) available draft)
    published <- newIORef Nothing
    generatedCount <- newIORef (0 :: Int)
    abandoned <- newIORef Nothing
    let request = ExcerptGenerationRequested requestIdentifier articleIdentifier revision
        message =
            ExcerptGenerationRequestedMessage
                (newEventEnvelope source (timestamp 1) actor correlation Nothing request)
        dependencies =
            GenerationDependencies
                { claimArticle = \_ -> pure (Right (Just (proofreadedContent proofreaded)))
                , generateExcerpt = \_ -> do
                    writeIORef generatedCount 1
                    pure (Right excerpt)
                , newIdentifier = pure (Right generatedIdentifier)
                , currentTime = pure (timestamp 2)
                , publishGenerated = writeIORef published . Just
                , abandonArticle = \failedRequest -> do
                    writeIORef abandoned (Just failedRequest)
                    pure (Right ())
                }
    handleGenerationMessage dependencies message
    sent <- readIORef published
    check "generated completion is sent" $ case sent of
        Just (ExcerptGeneratedMessage (EventEnvelope identifier _ _ _ _ payload)) ->
            identifier == generatedIdentifier
                && payload.request == requestIdentifier
                && payload.article == articleIdentifier
                && payload.expectedRevision == revision
                && payload.excerpt == excerpt
        Nothing -> False

    writeIORef published Nothing
    handleGenerationMessage
        dependencies{claimArticle = \_ -> pure (Right Nothing)}
        message
    check "obsolete request does not publish" . (== Nothing) =<< readIORef published
    check "obsolete request does not call AI" . (== 1) =<< readIORef generatedCount

    handleGenerationMessage
        dependencies
            { claimArticle =
                \_ -> pure (Left (createProcessingTargetChanged "Article" "changed"))
            }
        message
    check "changed target is acknowledged without AI" . (== 1) =<< readIORef generatedCount

    failedClaim <- try (handleGenerationMessage
        dependencies
            { claimArticle =
                \_ -> pure (Left (createServiceUnavailable "ArticleDO" "failure"))
            }
        message) :: IO (Either DomainError ())
    check "DO claim failure is retried" (either (const True) (const False) failedClaim)

    let unavailable =
            dependencies
                { generateExcerpt =
                    \_ -> pure (Left (createServiceUnavailable "WorkersAI" "failure"))
                }
    failed <- try (handleGenerationMessage unavailable message)
        :: IO (Either DomainError ())
    check "AI failure is retried" $ case failed of
        Left _ -> True
        Right _ -> False
    check "failed generation does not publish" . (== Nothing) =<< readIORef published

    let identifierUnavailable =
            dependencies
                { newIdentifier = pure (Left (createServiceUnavailable "Identifier" "failure"))
                }
    identifierFailed <- try (handleGenerationMessage identifierUnavailable message)
        :: IO (Either DomainError ())
    check "identifier failure is retried" $ case identifierFailed of
        Left _ -> True
        Right _ -> False
    check "identifier failure does not publish" . (== Nothing) =<< readIORef published

    handleGenerationDeadLetterMessage dependencies message
    check "dead letter releases the matching job" . (== Just request) =<< readIORef abandoned
    check "dead letter does not call AI" . (== 1) =<< readIORef generatedCount

    failedAbandon <- try (handleGenerationDeadLetterMessage
        dependencies
            { abandonArticle = \_ ->
                pure (Left (createServiceUnavailable "ArticleDO" "failure"))
            }
        message) :: IO (Either DomainError ())
    check "failed dead-letter release is retried" $ case failedAbandon of
        Left _ -> True
        Right _ -> False
