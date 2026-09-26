{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.Completion (
    applyGeneratedExcerpt,
    applyGeneratedExcerptWith,
) where

import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLLimits (..),
    sqlExec,
 )
import "article" UseCase.PrepareToPublish qualified as Prepare
import "shared" Shared.Domain.Error (DomainError, createTransactionOutcomeUnknown)
import "shared" Shared.Domain.Excerpt (excerptText)
import "shared" Shared.Infrastructure.Transaction (
    TransactionDriver (..),
    TransactionOutcome (..),
    runTransactionInContext,
 )
import "shared" Shared.UseCase.Command (Command (..), newCausation)
import "shared" Shared.UseCase.Event (EventEnvelope (..), eventIdentifierText)
import Infrastructure.Article.DurableObject.GenerationJob (
    GenerationFinalization,
    completeGenerationWith,
 )
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL)
import Infrastructure.Article.DurableObject.Transaction (ArticleTransactionContext (..))
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerated (..),
    ExcerptGeneratedMessage (..),
 )

applyGeneratedExcerpt ::
    TransactionDriver ArticleTransactionContext IO ->
    Prepare.Dependencies ArticleTransactionContext IO ->
    ExcerptGeneratedMessage ->
    IO (Either DomainError GenerationFinalization)
applyGeneratedExcerpt driver =
    applyGeneratedExcerptWith driver execute
  where
    execute context =
        sqlExec context.storage
            SQLLimits
                { maximumRows = 2
                , maximumBytes = 16777216
                , maximumStatements = 1
                }

-- Job finalization and PrepareToPublish share the same transaction context.
-- The use case's public entrypoint would open a nested transaction here.
applyGeneratedExcerptWith ::
    TransactionDriver context IO ->
    (context -> ExecuteSQL) ->
    Prepare.Dependencies context IO ->
    ExcerptGeneratedMessage ->
    IO (Either DomainError GenerationFinalization)
applyGeneratedExcerptWith driver execute dependencies
    (ExcerptGeneratedMessage
        (EventEnvelope sourceIdentifier occurredAt actor correlation _ generated)) =
        case newCausation (eventIdentifierText sourceIdentifier) of
            Left err -> pure (Left err)
            Right causation -> do
                let command =
                        Command
                            { payload =
                                Prepare.ApplyGeneratedExcerpt
                                    generated.article
                                    (excerptText generated.excerpt)
                            , timestamp = occurredAt
                            , actor = actor
                            , correlation = correlation
                            , causation = Just causation
                            }
                outcome <- withTransaction driver $ \context ->
                    completeGenerationWith
                        (execute context)
                        generated
                        ( fmap (fmap (const ())) $
                            runTransactionInContext
                                context
                                (Prepare.prepareToPublishInTransaction dependencies command)
                        )
                pure $ case outcome of
                    Committed result -> Right result
                    RolledBack err -> Left err
                    OutcomeUnknown reason ->
                        Left (createTransactionOutcomeUnknown "ArticleGeneration" reason)
