module Article.Worker.Completion.Composition (
    completionWorkerHandler,
) where

import Article.Worker.Completion.Contract (applyGeneratedExcerpt)
import Article.Worker.Completion.Env (CompletionWorkerEnv)
import Article.Worker.Excerpt.DeadLetter (abandonGenerationRequest)
import Cloudflare.Workers.Binding.Var (Var, unVar)
import Cloudflare.Workers.Entrypoint.Queue (QueueBatch (..), QueueConsumer)
import Cloudflare.Workers.Env (getBinding, getDurableObjectNamespace)
import Control.Exception (throwIO)
import Data.Proxy (Proxy (Proxy))
import Presentation.Handler.Queue.ExcerptCompletion (
    CompletionDependencies (..),
    handleCompletionBatch,
    handleCompletionDeadLetterBatch,
 )
import "shared" Shared.Domain.Error (createUnexpectedError)

completionWorkerHandler :: QueueConsumer CompletionWorkerEnv
completionWorkerHandler batch environment _context =
    if batch.queueBatchQueueName == unVar completionQueueName
        then handleCompletionBatch dependencies batch
        else
            if batch.queueBatchQueueName == unVar completionDeadLetterQueueName
                then handleCompletionDeadLetterBatch dependencies batch
                else throwIO (createUnexpectedError "ArticleCompletion" "unexpected queue")
  where
    dependencies =
        CompletionDependencies
            { applyGeneratedExcerpt = applyGeneratedExcerpt articleNamespace
            , abandonGeneration = abandonGenerationRequest articleNamespace
            }
    articleNamespace =
        getDurableObjectNamespace (Proxy @"ARTICLE_DO") environment
    completionQueueName =
        getBinding (Proxy @"COMPLETION_QUEUE_NAME") environment :: Var
    completionDeadLetterQueueName =
        getBinding (Proxy @"COMPLETION_DLQ_NAME") environment :: Var
