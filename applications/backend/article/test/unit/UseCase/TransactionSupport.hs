module UseCase.TransactionSupport where

import Data.IORef
import Data.Set (Set)
import Domain.Article (Article (..), ArticleIdentifier)
import Domain.Article.Common (ExtractImageReferences, ImageReference)
import Domain.Article.Criteria qualified as Criteria
import Domain.Article.Event (ArticleDraftStarted)
import Domain.Article.Published (PublishedArticle)
import Shared.Domain.Common.Transaction
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Slug (Slug)
import Shared.Infrastructure.Transaction
import Shared.UseCase.Command (Command (..))
import UseCase.AmendDraft qualified as AmendDraft
import UseCase.BrowseArticlesForAdmin qualified as BrowseArticlesForAdmin
import UseCase.BrowseArticlesForReader qualified as BrowseArticlesForReader
import UseCase.CheckSlugAvailability qualified as CheckSlugAvailability
import UseCase.DiscardArticle qualified as DiscardArticle
import UseCase.JotDown qualified as JotDown
import UseCase.LegacyPersistence qualified as Legacy
import UseCase.PrepareToPublish qualified as PrepareToPublish
import UseCase.Proofread qualified as Proofread
import UseCase.Publish qualified as Publish
import UseCase.ReadArticle qualified as ReadArticle
import UseCase.Reading (ArticleFilter, Criteria)
import UseCase.ResumePublication qualified as ResumePublication
import UseCase.TakeDown qualified as TakeDown
import UseCase.ViewArticleForAdmin qualified as ViewArticleForAdmin

-- Test-only recording adapters preserve behavioral regression assertions.
-- Atomicity and version tracking are tested separately with the memory driver.
data RecordingContext = RecordingContext
    { pending :: IORef (Maybe Article)
    , amendment :: IORef (Maybe (Legacy.LoadedArticle IO))
    , proofreading :: IORef (Maybe (Legacy.LoadedForProofreading IO))
    , publication :: IORef (Maybe (Legacy.LoadedForPublication IO))
    , takingDown :: IORef (Maybe (Legacy.LoadedForTakeDown IO))
    , resumption :: IORef (Maybe (Legacy.LoadedForResumption IO))
    , preparation :: IORef (Maybe (Legacy.LoadedForPreparation IO))
    , discard :: IORef (Maybe (Legacy.LoadedForDiscard IO))
    }
type TestTx = Transaction RecordingContext IO

recordingManager :: TransactionManager RecordingContext IO
recordingManager = newTransactionManager $ TransactionDriver $ \callback -> do
    context <-
        RecordingContext
            <$> newIORef Nothing
            <*> newIORef Nothing
            <*> newIORef Nothing
            <*> newIORef Nothing
            <*> newIORef Nothing
            <*> newIORef Nothing
            <*> newIORef Nothing
            <*> newIORef Nothing
    outcome <- callback context
    pure (either RolledBack Committed outcome)

remember :: IORef (Maybe a) -> IO (Either DomainError (Maybe a)) -> IO (Either DomainError (Maybe a))
remember ref action = do
    result <- action
    case result of
        Right value -> writeIORef ref value
        Left _ -> pure ()
    pure result

required :: IORef (Maybe a) -> IO a
required ref = readIORef ref >>= maybe (fail "missing fixture value") pure

stage :: Article -> TestTx ()
stage article = transactionAction $ \context -> writeIORef context.pending (Just article) >> pure (Right ())

jotDownDependencies ::
    IO (Either DomainError ArticleIdentifier) ->
    ExtractImageReferences ->
    Legacy.PersistDraft IO '[ArticleDraftStarted] ->
    JotDown.Dependencies RecordingContext IO
jotDownDependencies generate extract persist = JotDown.Dependencies recordingManager generate extract stage append
  where
    append metadata events = transactionAction $ \context -> do
        article <- required context.pending
        case article of
            Unvalidated draft -> persist metadata draft events
            _ -> fail "expected new draft"

amendDraftDependencies ::
    Legacy.LoadArticleForAmendment IO -> (ExtractImageReferences) -> AmendDraft.Dependencies RecordingContext IO
amendDraftDependencies loader extract = AmendDraft.Dependencies recordingManager find stage append extract
  where
    find identifier = transactionAction $ \context ->
        fmap (fmap (fmap (\value -> value.article))) (remember context.amendment (loader identifier))
    append metadata events = transactionAction $ \context -> do
        source <- required context.amendment
        article <- required context.pending
        case article of
            Unvalidated value -> source.persistAmendment metadata value events
            _ -> fail "unexpected state"

proofreadDependencies ::
    Legacy.LoadForProofreading IO ->
    (Set ImageReference -> IO (Either DomainError (Set ImageReference))) ->
    Proofread.Dependencies RecordingContext IO
proofreadDependencies loader images = Proofread.Dependencies recordingManager find stage append images
  where
    find identifier = transactionAction $ \context ->
        fmap (fmap (fmap (\value -> value.article))) (remember context.proofreading (loader identifier))
    append metadata events = transactionAction $ \context -> do
        source <- required context.proofreading
        article <- required context.pending
        case article of
            Proofreaded value -> source.persistProofreading metadata value events
            _ -> fail "unexpected state"

publishDependencies ::
    Legacy.LoadForPublication IO -> Publish.Dependencies RecordingContext IO
publishDependencies loader = Publish.Dependencies recordingManager find stage append
  where
    find identifier = transactionAction $ \context ->
        fmap (fmap (fmap (\value -> value.article))) (remember context.publication (loader identifier))
    append metadata events = transactionAction $ \context -> do
        source <- required context.publication
        article <- required context.pending
        case article of
            Published value -> source.persistPublication metadata value events
            _ -> fail "unexpected state"

takeDownDependencies ::
    Legacy.LoadForTakeDown IO -> TakeDown.Dependencies RecordingContext IO
takeDownDependencies loader = TakeDown.Dependencies recordingManager find stage append
  where
    find identifier = transactionAction $ \context ->
        fmap (fmap (fmap (\value -> value.article))) (remember context.takingDown (loader identifier))
    append metadata events = transactionAction $ \context -> do
        source <- required context.takingDown
        article <- required context.pending
        case article of
            Private value -> source.persistTakeDown metadata value events
            _ -> fail "unexpected state"

resumePublicationDependencies ::
    Command () -> Legacy.LoadForResumption IO -> ResumePublication.Dependencies RecordingContext IO
resumePublicationDependencies metadata loader = ResumePublication.Dependencies recordingManager find persist
  where
    find identifier = transactionAction $ \context ->
        fmap (fmap (fmap (\value -> value.article))) (remember context.resumption (loader identifier))
    persist (Ready article) = transactionAction $ \context -> do
        source <- required context.resumption
        source.persistResumption metadata article (Events [])
    persist _ = transactionAction (\_ -> fail "expected ready article")

prepareToPublishDependencies ::
    Command () ->
    Legacy.LoadForPreparation IO ->
    Legacy.LoadForPreparation IO ->
    Bool ->
    PrepareToPublish.Dependencies RecordingContext IO
prepareToPublishDependencies metadata generated revised generation =
    PrepareToPublish.Dependencies recordingManager find persist append
  where
    loader = if generation then generated else revised
    find identifier = transactionAction $ \context ->
        fmap (fmap (fmap (\value -> value.article))) (remember context.preparation (loader identifier))
    persist article@(Ready ready)
        | generation = stage article
        | otherwise = transactionAction $ \context -> do
            source <- required context.preparation
            source.persistPreparation metadata ready (Events [])
    persist _ = transactionAction (\_ -> fail "expected ready article")
    append supplied events = transactionAction $ \context -> do
        source <- required context.preparation
        article <- required context.pending
        case article of
            Ready ready -> source.persistPreparation supplied ready events
            _ -> fail "expected ready article"

prepareToPublish ::
    (Bool -> PrepareToPublish.Dependencies RecordingContext IO) ->
    PrepareToPublish.PrepareToPublishCommand ->
    IO (Either DomainError PrepareToPublish.PrepareToPublishResult)
prepareToPublish dependencies command =
    PrepareToPublish.prepareToPublish (dependencies generation) command
  where
    generation = case command.payload of
        PrepareToPublish.ApplyGeneratedExcerpt{} -> True
        PrepareToPublish.ReviseExcerpt{} -> False

discardArticleDependencies :: Legacy.LoadForDiscard IO -> DiscardArticle.Dependencies RecordingContext IO
discardArticleDependencies loader = DiscardArticle.Dependencies recordingManager find (const (pure ())) append
  where
    find identifier = transactionAction $ \context ->
        fmap (fmap (fmap (\value -> value.article))) (remember context.discard (loader identifier))
    append metadata events = transactionAction $ \context -> do
        source <- required context.discard
        source.commitDiscard metadata events

viewArticleForAdminDependencies ::
    (ArticleIdentifier -> IO (Either DomainError (Maybe Article))) ->
    ViewArticleForAdmin.Dependencies RecordingContext IO
viewArticleForAdminDependencies find =
    ViewArticleForAdmin.Dependencies
        recordingManager
        (\identifier -> transactionAction (\_ -> find identifier))

readArticleDependencies ::
    (Slug -> IO (Either DomainError (Maybe Article))) ->
    ReadArticle.Dependencies RecordingContext IO
readArticleDependencies find =
    ReadArticle.Dependencies
        recordingManager
        (\slug -> transactionAction (\_ -> find slug))

browseArticlesForAdminDependencies ::
    (ArticleFilter -> Criteria -> IO (Either DomainError (Int, [Article]))) ->
    BrowseArticlesForAdmin.Dependencies RecordingContext IO
browseArticlesForAdminDependencies search =
    BrowseArticlesForAdmin.Dependencies
        recordingManager
        (\criteria -> transactionAction (\_ -> search (Criteria.status criteria) criteria))

browseArticlesForReaderDependencies ::
    (Criteria -> IO (Either DomainError (Int, [PublishedArticle]))) ->
    BrowseArticlesForReader.Dependencies RecordingContext IO
browseArticlesForReaderDependencies search =
    BrowseArticlesForReader.Dependencies
        recordingManager
        (\criteria -> transactionAction (\_ -> search criteria))

checkSlugAvailabilityDependencies ::
    (ArticleIdentifier -> IO (Either DomainError (Maybe Article))) ->
    (Slug -> IO (Either DomainError (Maybe ArticleIdentifier))) ->
    CheckSlugAvailability.Dependencies RecordingContext IO
checkSlugAvailabilityDependencies find owner =
    CheckSlugAvailability.Dependencies
        recordingManager
        (\identifier -> transactionAction (\_ -> find identifier))
        (\slug -> transactionAction (\_ -> owner slug))
