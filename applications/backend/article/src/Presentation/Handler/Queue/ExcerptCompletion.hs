module Presentation.Handler.Queue.ExcerptCompletion (
    CompletionOutcome (..),
    CompletionDependencies (..),
    handleCompletionMessage,
    handleCompletionBatch,
) where

import Cloudflare.Workers.Entrypoint.Queue (QueueBatch)
import Cloudflare.Workers.Entrypoint.Queue.Typed (consumeJSONMessages)
import Control.Exception (throwIO)
import Infrastructure.Article.Queue.ExcerptGeneration (ExcerptGeneratedMessage)
import "shared" Shared.Domain.Error (DomainError (..))

data CompletionOutcome
    = ExcerptApplied
    | ExcerptAlreadyApplied
    | ExcerptNoLongerRequired
    deriving stock (Show, Eq)

newtype CompletionDependencies = CompletionDependencies
    { applyGeneratedExcerpt ::
        ExcerptGeneratedMessage -> IO (Either DomainError CompletionOutcome)
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
