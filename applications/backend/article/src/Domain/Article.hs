module Domain.Article (
    module Domain.Article.Common,
    Article (..),
    articleIdentifier,
    FindArticle,
    PersistArticle,
    TerminateArticle,
) where

import Domain.Article.Common
import Domain.Article.Draft (
    ProofreadedDraft,
    ReadyToPublish,
    UnvalidatedDraft,
    draftIdentifier,
 )
import Domain.Article.Private (PrivateArticle)
import Domain.Article.Published (PublishedArticle)
import Shared.Domain.Error (DomainError)

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

type FindArticle m = ArticleIdentifier -> m (Either DomainError (Maybe Article))
type PersistArticle m = Article -> m (Either DomainError ())
type TerminateArticle m = ArticleIdentifier -> m (Either DomainError ())
