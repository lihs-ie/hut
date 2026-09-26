module Presentation.Handler.Queue.ExcerptCompletion (
    CompletionOutcome (..),
    CompletionDependencies (..),
    handleCompletionMessage,
    handleCompletionBatch,
    handleCompletionDeadLetterMessage,
    handleCompletionDeadLetterBatch,
) where

import Cloudflare.Workers.Entrypoint.Queue (QueueBatch)
import Cloudflare.Workers.Entrypoint.Queue.Typed (consumeJSONMessages)
import Control.Exception (throwIO)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerated (..),
    ExcerptGeneratedMessage (..),
    ExcerptGenerationRequested (..),
 )
import "shared" Shared.Domain.Error (DomainError (..))
import "shared" Shared.UseCase.Event (EventEnvelope (..))

data CompletionOutcome
    = ExcerptApplied
    | ExcerptAlreadyApplied
    | ExcerptNoLongerRequired
    deriving stock (Show, Eq)

data CompletionDependencies = CompletionDependencies
    { applyGeneratedExcerpt ::
        ExcerptGeneratedMessage -> IO (Either DomainError CompletionOutcome)
    , abandonGeneration ::
        ExcerptGenerationRequested -> IO (Either DomainError ())
    }

handleCompletionMessage :: CompletionDependencies -> ExcerptGeneratedMessage -> IO ()
handleCompletionMessage dependencies message = do
    outcome <- dependencies.applyGeneratedExcerpt message
    case outcome of
        Right _ -> pure ()
        Left (ProcessingTargetChanged _) -> pure ()
        Left err -> throwIO err

handleCompletionBatch :: CompletionDependencies -> QueueBatch -> IO ()
handleCompletionBatch dependencies =
    consumeJSONMessages
        (\_ message -> handleCompletionMessage dependencies message)

handleCompletionDeadLetterMessage ::
    CompletionDependencies -> ExcerptGeneratedMessage -> IO ()
handleCompletionDeadLetterMessage dependencies
    (ExcerptGeneratedMessage (EventEnvelope _ _ _ _ _ generated)) = do
        let request =
                ExcerptGenerationRequested
                    generated.request
                    generated.article
                    generated.expectedRevision
        result <- dependencies.abandonGeneration request
        either throwIO pure result

handleCompletionDeadLetterBatch :: CompletionDependencies -> QueueBatch -> IO ()
handleCompletionDeadLetterBatch dependencies =
    consumeJSONMessages
        (\_ message -> handleCompletionDeadLetterMessage dependencies message)
