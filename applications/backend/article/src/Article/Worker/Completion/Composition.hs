module Article.Worker.Completion.Composition (
    completionWorkerHandler,
) where

import Article.Worker.Completion.Contract (applyGeneratedExcerpt)
import Article.Worker.Completion.Env (CompletionWorkerEnv)
import Cloudflare.Workers.Entrypoint.Queue (QueueConsumer)
import Cloudflare.Workers.Env (getDurableObjectNamespace)
import Data.Proxy (Proxy (Proxy))
import Presentation.Handler.Queue.ExcerptCompletion (
    CompletionDependencies (..),
    handleCompletionBatch,
 )

completionWorkerHandler :: QueueConsumer CompletionWorkerEnv
completionWorkerHandler batch environment _context =
    handleCompletionBatch
        (CompletionDependencies (applyGeneratedExcerpt articleNamespace))
        batch
  where
    articleNamespace =
        getDurableObjectNamespace (Proxy @"ARTICLE_DO") environment
