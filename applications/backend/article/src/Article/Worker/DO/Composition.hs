{-# LANGUAGE TypeApplications #-}

module Article.Worker.DO.Composition (
    articleDOHandler,
) where

import Article.Worker.DO.Env (ArticleDOEnv)
import Cloudflare.Workers.Binding.DurableObject (
    DurableObjectStorage,
    doStorageTransactionWith,
 )
import Cloudflare.Workers.Binding.ServiceBinding (ServiceBinding)
import Cloudflare.Workers.Binding.DurableObject.SQL (
    SQLLimits (..),
    sqlExec,
 )
import Cloudflare.Workers.Entrypoint.Fetch (FetchHandler)
import Cloudflare.Workers.Env (getBinding)
import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (
    Response (..),
    ResponseBody (ResponseBodyBytes),
    Status (Status),
    requestPath,
 )
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Infrastructure.Article.DurableObject.Codec (articleCodec)
import Infrastructure.Article.DurableObject.Completion (applyGeneratedExcerpt)
import Infrastructure.Article.DurableObject.GenerationJob (
    GenerationFinalization (..),
    initializeGenerationJobSchemaWith,
 )
import Infrastructure.Article.DurableObject.ReadyOutbox (appendReadyEvents)
import Infrastructure.Article.DurableObject.ProofreadOutbox (
    appendProofreadOutbox,
    recordRegenerationRequest,
 )
import Infrastructure.Article.DurableObject.Repository (
    findArticle,
    initializeSchema,
    persistArticle,
 )
import Infrastructure.Article.DurableObject.Transaction (
    ArticleTransactionContext (..),
    newArticleTransactionDriver,
 )
import Infrastructure.Article.Media.ImageAvailability qualified as Media
import Infrastructure.Article.Queue.ExcerptGeneration (
    GenerationRequestIdentifier,
    newGenerationRequestIdentifier,
    generationRequestIdentifierText,
 )
import Presentation.Handler.API.Metadata (MetadataDependencies (..))
import Presentation.Handler.API.Proofread (ProofreadHandlerDependencies (..))
import Presentation.Handler.API.RequestExcerptRegeneration (
    RegenerationHandlerDependencies (..),
 )
import Presentation.Handler.DO.ExcerptComplete (
    CompleteOutcome (..),
    handleExcerptComplete,
 )
import Presentation.Handler.DO.ExcerptClaim (handleExcerptClaim)
import Presentation.Server.API (APIServerDependencies (..), articleAPIServer)
import Shared.Domain.Error (DomainError, createUnexpectedError)
import Shared.Domain.Identifier (ulidText)
import Shared.FFI.SecureRandom (secureRandomBytes)
import Shared.Infrastructure.Transaction (newTransactionManager, transactionAction)
import Shared.UseCase.Command (Command (..))
import Shared.UseCase.Event (EventIdentifier, newEventIdentifier)
import Shared.UseCase.Identifier (
    IdentifierGenerationDependencies (IdentifierGenerationDependencies),
    generateULID,
 )
import "article" UseCase.PrepareToPublish qualified as Prepare
import "article" UseCase.Proofread qualified as Proof
import "article" UseCase.RequestExcerptRegeneration qualified as Regeneration

articleDOHandler :: FetchHandler ArticleDOEnv
articleDOHandler request environment context = do
    initialized <- initializeSchema storage
    jobs <- case initialized of
        Left err -> pure (Left err)
        Right () -> initializeGenerationJobSchemaWith (sqlExec storage claimSQLLimits)
    case jobs of
        Left _ -> pure (emptyResponse 500)
        Right () -> case requestPath request of
            "/internal/excerpt-generation/claim" ->
                handleExcerptClaim
                    (doStorageTransactionWith storage)
                    (sqlExec storage claimSQLLimits)
                    request
            "/internal/excerpt-generation/complete" -> do
                driver <- newArticleTransactionDriver storage
                let dependencies = Prepare.Dependencies
                        { Prepare.transactionManager = newTransactionManager driver
                        , Prepare.findArticle = \identifier -> transactionAction $ \transactionContext ->
                            findArticle transactionContext.storage transactionContext.versions articleCodec identifier
                        , Prepare.persistArticle = \article -> transactionAction $ \transactionContext ->
                            persistArticle transactionContext.storage transactionContext.versions articleCodec article
                        , Prepare.appendEvents = appendReadyEvents generateEventIdentifier
                        }
                    apply message = do
                        result <- applyGeneratedExcerpt driver dependencies message
                        pure $ fmap completionOutcome result
                handleExcerptComplete apply request
            path | "/admin/" `Text.isPrefixOf` path ->
                articleAPIServer request (adminDependencies storage media) context
            _ -> pure (emptyResponse 404)
  where
    storage :: DurableObjectStorage
    storage = getBinding (Proxy @"STORAGE") environment
    media :: ServiceBinding
    media = getBinding (Proxy @"MEDIA_API") environment

adminDependencies :: DurableObjectStorage -> ServiceBinding -> APIServerDependencies
adminDependencies storage media =
    APIServerDependencies
        { proofread = ProofreadHandlerDependencies
            { metadata
            , execute = \command -> do
                driver <- newArticleTransactionDriver storage
                let dependencies = Proof.Dependencies
                        { Proof.transactionManager = newTransactionManager driver
                        , Proof.findArticle = findInTransaction
                        , Proof.persistArticle = persistInTransaction
                        , Proof.appendEvents =
                            appendProofreadOutbox
                                generateRequestIdentifier
                                generateEventIdentifier
                        , Proof.findAvailableImages =
                            Media.findAvailableImages media command.actor command.correlation
                        }
                fmap (fmap (const ())) (Proof.proofread dependencies command)
            }
        , requestExcerptRegeneration = RegenerationHandlerDependencies
            { metadata
            , execute = \command -> do
                driver <- newArticleTransactionDriver storage
                let dependencies = Regeneration.Dependencies
                        { Regeneration.transactionManager = newTransactionManager driver
                        , Regeneration.findArticle = findInTransaction
                        , Regeneration.recordRegenerationRequest =
                            recordRegenerationRequest
                                generateRequestIdentifier
                                generateEventIdentifier
                        }
                result <- Regeneration.requestExcerptRegeneration dependencies command
                pure $ fmap
                    (generationRequestIdentifierText . (.requestIdentifier))
                    result
            }
        }
  where
    metadata = MetadataDependencies
        { currentTime = Right <$> getCurrentTime
        , newCorrelation = generateULIDText
        }
    findInTransaction identifier = transactionAction $ \context ->
        findArticle context.storage context.versions articleCodec identifier
    persistInTransaction article = transactionAction $ \context ->
        persistArticle context.storage context.versions articleCodec article

claimSQLLimits :: SQLLimits
claimSQLLimits =
    SQLLimits
        { maximumRows = 2
        , maximumBytes = 16777216
        , maximumStatements = 1
        }

completionOutcome :: GenerationFinalization -> CompleteOutcome
completionOutcome GenerationFinalized = ExcerptCompleted
completionOutcome GenerationFinalizationTerminalAck = CompletionObsolete

generateULIDText :: IO (Either DomainError Text)
generateULIDText = do
    generated <-
        generateULID
            ( IdentifierGenerationDependencies
                (Right <$> getCurrentTime)
                secureRandomBytes
            )
    pure (ulidText <$> generated)

generateEventIdentifier :: IO (Either DomainError EventIdentifier)
generateEventIdentifier = do
    generated <- generateULIDText
    pure $ generated >>= \value ->
        either
            (const (Left (createUnexpectedError "ArticleEvent" "generated identifier was invalid")))
            Right
            (newEventIdentifier value)

generateRequestIdentifier ::
    IO (Either DomainError GenerationRequestIdentifier)
generateRequestIdentifier = fmap (>>= newGenerationRequestIdentifier) generateULIDText

emptyResponse :: Int -> Response
emptyResponse status =
    Response
        { responseStatus = Status status
        , responseHeaders = headersFromList [("cache-control", "no-store")]
        , responseBody = ResponseBodyBytes ""
        }
