module Article.Worker.Excerpt.Composition (
    excerptWorkerHandler,
) where

import Article.Worker.Excerpt.Contract (claimArticleContent)
import Article.Worker.Excerpt.Env (ExcerptWorkerEnv)
import Cloudflare.Workers.Binding.Queue (
    QueueProducer,
    queueSend,
    queueSendDefaultOptions,
 )
import Cloudflare.Workers.Binding.WorkersAI (WorkersAI)
import Cloudflare.Workers.Entrypoint.Queue (QueueConsumer)
import Cloudflare.Workers.Env (getBinding, getDurableObjectNamespace)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.Proxy (Proxy (Proxy))
import Data.Time (getCurrentTime)
import Infrastructure.Article.Excerpt.WorkersAI qualified as WorkersAI
import Presentation.Handler.Queue.ExcerptGeneration (
    GenerationDependencies (..),
    handleGenerationBatch,
 )
import "shared" Shared.Domain.Identifier (ulidText)
import "shared" Shared.Domain.Error (DomainError, createUnexpectedError)
import "shared" Shared.FFI.SecureRandom (secureRandomBytes)
import "shared" Shared.UseCase.Event (EventIdentifier, newEventIdentifier)
import "shared" Shared.UseCase.Identifier (
    IdentifierGenerationDependencies (IdentifierGenerationDependencies),
    generateULID,
 )

excerptWorkerHandler :: QueueConsumer ExcerptWorkerEnv
excerptWorkerHandler batch environment _context =
    handleGenerationBatch dependencies batch
  where
    dependencies =
        GenerationDependencies
            { claimArticle = claimArticleContent articleNamespace
            , generateExcerpt = WorkersAI.generateExcerpt ai
            , newIdentifier = generateEventIdentifier
            , currentTime = getCurrentTime
            , publishGenerated = \message ->
                queueSend completionQueue
                    (Lazy.toStrict (encode message))
                    queueSendDefaultOptions
            }
    articleNamespace =
        getDurableObjectNamespace (Proxy @"ARTICLE_DO") environment
    ai = getBinding (Proxy @"AI") environment :: WorkersAI
    completionQueue =
        getBinding
            (Proxy @"ARTICLE_EXCERPT_COMPLETION_QUEUE")
            environment :: QueueProducer

generateEventIdentifier :: IO (Either DomainError EventIdentifier)
generateEventIdentifier = do
    generated <-
        generateULID
            ( IdentifierGenerationDependencies
                (Right <$> getCurrentTime)
                secureRandomBytes
            )
    pure $ generated >>= \value ->
        either
            (const (Left (createUnexpectedError "ArticleEvent" "generated identifier was invalid")))
            Right
            (newEventIdentifier (ulidText value))
