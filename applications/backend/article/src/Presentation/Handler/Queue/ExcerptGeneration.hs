module Presentation.Handler.Queue.ExcerptGeneration (
    GenerationDependencies (..),
    handleGenerationMessage,
    handleGenerationBatch,
) where

import Cloudflare.Workers.Entrypoint.Queue (QueueBatch)
import Cloudflare.Workers.Entrypoint.Queue.Typed (consumeJSONMessages)
import Control.Exception (throwIO)
import Data.Time (UTCTime)
import "article" Domain.Article.Common (ProofreadedContent)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerated (..),
    ExcerptGeneratedMessage (..),
    ExcerptGenerationRequested (..),
    ExcerptGenerationRequestedMessage (..),
 )
import "shared" Shared.Domain.Error (DomainError (..))
import "shared" Shared.Domain.Excerpt (Excerpt)
import "shared" Shared.UseCase.Context (newCausation)
import "shared" Shared.UseCase.Event (
    EventEnvelope (..),
    EventIdentifier,
    eventIdentifierText,
    newEventEnvelope,
 )

data GenerationDependencies = GenerationDependencies
    { claimArticle ::
        ExcerptGenerationRequested ->
        IO (Either DomainError (Maybe ProofreadedContent))
    , generateExcerpt :: ProofreadedContent -> IO (Either DomainError Excerpt)
    , newIdentifier :: IO (Either DomainError EventIdentifier)
    , currentTime :: IO UTCTime
    , publishGenerated :: ExcerptGeneratedMessage -> IO ()
    }

handleGenerationMessage ::
    GenerationDependencies -> ExcerptGenerationRequestedMessage -> IO ()
handleGenerationMessage dependencies
    (ExcerptGenerationRequestedMessage
        (EventEnvelope sourceIdentifier _ actor correlation _ request)) = do
        claimed <- dependencies.claimArticle request
        case claimed of
            Left (ProcessingTargetChanged _) -> pure ()
            Left err -> throwIO err
            Right Nothing -> pure ()
            Right (Just article) -> do
                excerpt <- dependencies.generateExcerpt article >>= either throwIO pure
                identifier <- dependencies.newIdentifier >>= either throwIO pure
                occurredAt <- dependencies.currentTime
                causation <-
                    either throwIO pure
                        (newCausation (eventIdentifierText sourceIdentifier))
                let generated =
                        ExcerptGenerated
                            request.identifier
                            request.article
                            request.expectedRevision
                            excerpt
                dependencies.publishGenerated
                    ( ExcerptGeneratedMessage
                        ( newEventEnvelope
                            identifier
                            occurredAt
                            actor
                            correlation
                            (Just causation)
                            generated
                        )
                    )

handleGenerationBatch :: GenerationDependencies -> QueueBatch -> IO ()
handleGenerationBatch dependencies =
    consumeJSONMessages
        (\_ message -> handleGenerationMessage dependencies message)
