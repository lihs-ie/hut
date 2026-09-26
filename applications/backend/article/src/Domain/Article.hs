module Domain.Article (
    module Domain.Article.Common,
    Article (..),
    articleIdentifier,
    FindArticle,
    PersistArticle,
    TerminateArticle,
    FindArticleBySlug,
    FindSlugOwner,
    SearchArticles,
    SearchPublishedArticles,
) where

import Domain.Article.Common
import Domain.Article.Criteria (Criteria)
import Domain.Article.Draft (
    ProofreadedDraft,
    ReadyToPublish,
    UnvalidatedDraft,
    draftIdentifier,
 )
import Domain.Article.Private (PrivateArticle)
import Domain.Article.Published (PublishedArticle)
import Shared.Domain.Slug (Slug)

data Article
    = Unvalidated UnvalidatedDraft
    | Proofreaded ProofreadedDraft
    | Ready ReadyToPublish
    | Published PublishedArticle
    | Private PrivateArticle
    deriving stock (Show, Eq)

articleIdentifier :: Article -> ArticleIdentifier
articleIdentifier (Unvalidated draft) = draftIdentifier draft
articleIdentifier (Proofreaded draft) = draftIdentifier draft
articleIdentifier (Ready draft) = draftIdentifier draft
articleIdentifier (Published article) = article.identifier
articleIdentifier (Private article) = article.identifier

type FindArticle m = ArticleIdentifier -> m (Maybe Article)
type PersistArticle m = Article -> m ()
type TerminateArticle m = ArticleIdentifier -> m ()
type FindArticleBySlug m = Slug -> m (Maybe Article)
type FindSlugOwner m = Slug -> m (Maybe ArticleIdentifier)

-- Count and page from the same snapshot. Return totals BEFORE pagination.
-- Admin: updatedAt DESC, identifier DESC; reader: publishedAt DESC, identifier DESC.
type SearchArticles m = Criteria -> m (Int, [Article])

-- Only PublishedOnly criteria are accepted; reject other selections.
type SearchPublishedArticles m = Criteria -> m (Int, [PublishedArticle])
