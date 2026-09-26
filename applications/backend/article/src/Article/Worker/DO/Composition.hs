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
import Cloudflare.Workers.Binding.Var (unVar)
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
import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Infrastructure.Article.DurableObject.Codec (articleCodec)
import Infrastructure.Article.DurableObject.DomainOutbox qualified as DomainOutbox
import Infrastructure.Article.DurableObject.Completion (applyGeneratedExcerpt)
import Infrastructure.Article.DurableObject.GenerationJob (
    GenerationFinalization (..),
    abandonGenerationWith,
    initializeGenerationJobSchemaWith,
 )
import Infrastructure.Article.DurableObject.ReadyOutbox (appendReadyEvents)
import Infrastructure.Article.DurableObject.ProofreadOutbox (
    appendProofreadOutbox,
    recordRegenerationRequest,
 )
import Infrastructure.Article.DurableObject.Query qualified as Query
import Infrastructure.Article.DurableObject.Repository (
    findArticle,
    initializeSchema,
    persistArticle,
    terminateArticle,
 )
import Infrastructure.Article.DurableObject.Transaction (
    ArticleTransactionContext (..),
    newArticleTransactionDriver,
 )
import Infrastructure.Article.Media.ImageAvailability qualified as Media
import Infrastructure.Article.Media.ImageReferences (extractManagedImageReferences)
import Infrastructure.Article.Queue.ExcerptGeneration (
    GenerationRequestIdentifier,
    newGenerationRequestIdentifier,
    generationRequestIdentifierText,
 )
import Presentation.Handler.API.Metadata (MetadataDependencies (..))
import Presentation.Handler.API.DraftWriting (DraftWritingDependencies (..))
import Presentation.Handler.API.Publication (PublicationHandlerDependencies (..))
import Presentation.Handler.API.Proofread (ProofreadHandlerDependencies (..))
import Presentation.Handler.API.Reading (ReadingHandlerDependencies (..))
import Presentation.Handler.API.RequestExcerptRegeneration (
    RegenerationHandlerDependencies (..),
 )
import Presentation.Handler.DO.ExcerptComplete (
    CompleteOutcome (..),
    handleExcerptComplete,
 )
import Presentation.Handler.DO.ExcerptAbandon (handleExcerptAbandon)
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
import "article" UseCase.BrowseArticlesForAdmin qualified as BrowseAdmin
import "article" UseCase.BrowseArticlesForReader qualified as BrowseReader
import "article" UseCase.CheckSlugAvailability qualified as CheckSlug
import "article" UseCase.ReadArticle qualified as ReadArticle
import "article" UseCase.ViewArticleForAdmin qualified as ViewAdmin
import "article" Domain.Article qualified as Article
import "article" UseCase.AmendDraft qualified as Amend
import "article" UseCase.DiscardArticle qualified as Discard
import "article" UseCase.JotDown qualified as JotDown
import "article" UseCase.Publish qualified as Publish
import "article" UseCase.ResumePublication qualified as Resume
import "article" UseCase.TakeDown qualified as TakeDown

articleDOHandler :: FetchHandler ArticleDOEnv
articleDOHandler request environment context = do
    initialized <- initializeSchema storage articleCodec
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
            "/internal/excerpt-generation/abandon" ->
                handleExcerptAbandon
                    (\generation -> do
                        outcome <- doStorageTransactionWith storage $ do
                            result <- abandonGenerationWith
                                (sqlExec storage claimSQLLimits)
                                generation
                            either throwIO pure result
                        pure (Right outcome)
                    )
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
                articleAPIServer request (adminDependencies storage media assetOrigin) context
            path | "/articles" == path || "/articles/" `Text.isPrefixOf` path ->
                articleAPIServer request (adminDependencies storage media assetOrigin) context
            _ -> pure (emptyResponse 404)
  where
    storage :: DurableObjectStorage
    storage = getBinding (Proxy @"STORAGE") environment
    media :: ServiceBinding
    media = getBinding (Proxy @"MEDIA_API") environment
    assetOrigin :: Text
    assetOrigin = maybe "" unVar (getBinding (Proxy @"MEDIA_ASSET_ORIGIN") environment)

adminDependencies :: DurableObjectStorage -> ServiceBinding -> Text -> APIServerDependencies
adminDependencies storage media assetOrigin =
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
        , reading = ReadingHandlerDependencies
            { metadata
            , browseAdmin = \command -> do
                manager <- newManager
                BrowseAdmin.browseArticlesForAdmin
                    BrowseAdmin.Dependencies
                        { BrowseAdmin.transactionManager = manager
                        , BrowseAdmin.searchArticles = \criteria -> transactionAction $ \context ->
                            Query.searchArticles context.storage context.versions articleCodec criteria
                        }
                    command
            , viewAdmin = \command -> do
                manager <- newManager
                ViewAdmin.viewArticleForAdmin
                    ViewAdmin.Dependencies
                        { ViewAdmin.transactionManager = manager
                        , ViewAdmin.findArticle = findInTransaction
                        }
                    command
            , checkSlug = \command -> do
                manager <- newManager
                CheckSlug.checkSlugAvailability
                    CheckSlug.Dependencies
                        { CheckSlug.transactionManager = manager
                        , CheckSlug.findArticle = findInTransaction
                        , CheckSlug.findSlugOwner = \slug -> transactionAction $ \context ->
                            Query.findSlugOwner context.storage slug
                        }
                    command
            , browseReader = \command -> do
                manager <- newManager
                BrowseReader.browseArticlesForReader
                    BrowseReader.Dependencies
                        { BrowseReader.transactionManager = manager
                        , BrowseReader.searchArticles = \criteria -> transactionAction $ \context ->
                            Query.searchPublishedArticles context.storage context.versions articleCodec criteria
                        }
                    command
            , readArticle = \command -> do
                manager <- newManager
                ReadArticle.readArticle
                    ReadArticle.Dependencies
                        { ReadArticle.transactionManager = manager
                        , ReadArticle.findArticleBySlug = \slug -> transactionAction $ \context ->
                            Query.findArticleBySlug context.storage context.versions articleCodec slug
                        }
                    command
            }
        , draftWriting = DraftWritingDependencies
            { metadata
            , jotDown = \command -> do
                manager <- newManager
                result <- JotDown.jotDown
                    JotDown.Dependencies
                        { JotDown.transactionManager = manager
                        , JotDown.newArticleIdentifier =
                            fmap (>>= Article.newArticleIdentifier) generateULIDText
                        , JotDown.extractImageReferences =
                            extractManagedImageReferences assetOrigin
                        , JotDown.persistArticle = persistInTransaction
                        , JotDown.appendEvents =
                            DomainOutbox.appendDraftStarted generateEventIdentifier
                        }
                    command
                pure (fmap (.article) result)
            , amendDraft = \command -> do
                manager <- newManager
                result <- Amend.amendDraft
                    Amend.Dependencies
                        { Amend.transactionManager = manager
                        , Amend.findArticle = findInTransaction
                        , Amend.persistArticle = persistInTransaction
                        , Amend.appendEvents =
                            DomainOutbox.appendDraftAmended generateEventIdentifier
                        , Amend.extractImageReferences =
                            extractManagedImageReferences assetOrigin
                        }
                    command
                pure (fmap (.article) result)
            , reviseExcerpt = \command -> do
                manager <- newManager
                result <- Prepare.prepareToPublish
                    Prepare.Dependencies
                        { Prepare.transactionManager = manager
                        , Prepare.findArticle = findInTransaction
                        , Prepare.persistArticle = persistInTransaction
                        , Prepare.appendEvents = appendReadyEvents generateEventIdentifier
                        }
                    command
                pure (fmap (.article) result)
            }
        , publication = PublicationHandlerDependencies
            { metadata
            , publish = \command -> do
                manager <- newManager
                result <- Publish.publish
                    Publish.Dependencies
                        { Publish.transactionManager = manager
                        , Publish.findArticle = findInTransaction
                        , Publish.persistArticle = persistInTransaction
                        , Publish.appendEvents =
                            DomainOutbox.appendPublished generateEventIdentifier
                        }
                    command
                pure (fmap (.article) result)
            , takeDown = \command -> do
                manager <- newManager
                result <- TakeDown.takeDown
                    TakeDown.Dependencies
                        { TakeDown.transactionManager = manager
                        , TakeDown.findArticle = findInTransaction
                        , TakeDown.persistArticle = persistInTransaction
                        , TakeDown.appendEvents =
                            DomainOutbox.appendTakenDown generateEventIdentifier
                        }
                    command
                pure (fmap (.article) result)
            , resumePublication = \command -> do
                manager <- newManager
                result <- Resume.resumePublication
                    Resume.Dependencies
                        { Resume.transactionManager = manager
                        , Resume.findArticle = findInTransaction
                        , Resume.persistArticle = persistInTransaction
                        }
                    command
                pure (fmap (.article) result)
            , discardArticle = \command -> do
                manager <- newManager
                result <- Discard.discardArticle
                    Discard.Dependencies
                        { Discard.transactionManager = manager
                        , Discard.findArticle = findInTransaction
                        , Discard.terminateArticle = \identifier -> transactionAction $ \context ->
                            terminateArticle context.storage context.versions identifier
                        , Discard.appendEvents =
                            DomainOutbox.appendDiscarded generateEventIdentifier
                        }
                    command
                pure (fmap (.article) result)
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
    newManager = newTransactionManager <$> newArticleTransactionDriver storage

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
