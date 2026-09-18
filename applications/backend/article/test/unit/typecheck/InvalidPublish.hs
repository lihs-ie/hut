module InvalidPublish where

import Data.Time (UTCTime)
import Domain.Article.Draft (UnvalidatedDraft)
import Domain.Article.Published (PublishedArticle, publish)
import Shared.Domain.Error (DomainError)

invalid :: UTCTime -> UnvalidatedDraft -> Either DomainError PublishedArticle
invalid = publish
