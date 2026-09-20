-- Fixtures for the pre-transaction behavioral regression tests only.
module UseCase.LegacyPersistence (
    PersistDraft,
    PersistArticle,
    LoadedForProofreading (..),
    LoadedForPreparation (..),
    LoadForProofreading,
    LoadForPreparation,
    LoadedForPublication (..),
    LoadedForTakeDown (..),
    LoadForPublication,
    LoadForTakeDown,
    LoadedForResumption (..),
    LoadForResumption,
    LoadedForDiscard (..),
    LoadForDiscard,
    CommitDiscard,
    LoadedArticle (..),
    LoadArticleForAmendment,
    commandContext,
) where

import Domain.Article (Article, ArticleIdentifier)
import Domain.Article.Draft (ProofreadedDraft, ReadyToPublish, UnvalidatedDraft)
import Domain.Article.Event (ArticleDiscarded, ArticleDraftAmended, ArticleProofreaded, ArticlePublished, ArticleReadyToPublish, ArticleTakenDown)
import Domain.Article.Private (PrivateArticle)
import Domain.Article.Published (PublishedArticle)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (Events)
import Shared.UseCase.Command (Command, commandContext)

-- One atomic operation: enforce Slug uniqueness, persist the article and append
-- enveloped events to the outbox. Failure must leave both stores unchanged.
-- The adapter creates envelope identifiers; it must not publish to a queue here.
type PersistArticle m article events =
    Command () -> article -> Events events -> m (Either DomainError ())

type PersistDraft m events = PersistArticle m UnvalidatedDraft events

data LoadedForResumption m = LoadedForResumption
    { article :: Article
    , persistResumption :: PersistArticle m ReadyToPublish '[]
    }

type LoadForResumption m =
    ArticleIdentifier -> m (Either DomainError (Maybe (LoadedForResumption m)))

-- Atomically delete the loaded article, release its Slug and append the outbox
-- event. Bound to the loaded identity AND revision; concurrent changes fail.
-- Do not delete Media images or their usage records here.
type CommitDiscard m =
    Command () -> Events '[ArticleDiscarded] -> m (Either DomainError ())

data LoadedForDiscard m = LoadedForDiscard
    { article :: Article
    , commitDiscard :: CommitDiscard m
    }

type LoadForDiscard m =
    ArticleIdentifier -> m (Either DomainError (Maybe (LoadedForDiscard m)))

data LoadedForPublication m = LoadedForPublication
    { article :: Article
    , persistPublication :: PersistArticle m PublishedArticle '[ArticlePublished]
    }

data LoadedForTakeDown m = LoadedForTakeDown
    { article :: Article
    , persistTakeDown :: PersistArticle m PrivateArticle '[ArticleTakenDown]
    }

type LoadForPublication m =
    ArticleIdentifier -> m (Either DomainError (Maybe (LoadedForPublication m)))

type LoadForTakeDown m =
    ArticleIdentifier -> m (Either DomainError (Maybe (LoadedForTakeDown m)))

data LoadedForProofreading m = LoadedForProofreading
    { article :: Article
    , persistProofreading :: PersistArticle m ProofreadedDraft '[ArticleProofreaded]
    }

data LoadedForPreparation m = LoadedForPreparation
    { article :: Article
    , persistPreparation :: PersistArticle m ReadyToPublish '[ArticleReadyToPublish]
    }

type LoadForProofreading m =
    ArticleIdentifier -> m (Either DomainError (Maybe (LoadedForProofreading m)))

type LoadForPreparation m =
    ArticleIdentifier -> m (Either DomainError (Maybe (LoadedForPreparation m)))

-- All loaded save functions are conditional on the loaded identity and revision.
-- Proofreading saves attach the resulting revision to the generation outbox item.
-- Initial preparation also retains the generation event's expected revision:
-- loading the latest revision must not silently replace that expectation.

data LoadedArticle m = LoadedArticle
    { article :: Article
    , -- Bound to the loaded identity and revision by the infrastructure adapter.
      -- A concurrent update/deletion must fail, never overwrite or recreate.
      persistAmendment :: PersistDraft m '[ArticleDraftAmended]
    }

type LoadArticleForAmendment m =
    ArticleIdentifier -> m (Either DomainError (Maybe (LoadedArticle m)))
