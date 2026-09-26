module ValidPublish where

import Data.Time (UTCTime)
import Domain.Article.Draft (ReadyToPublish)
import Domain.Article.Published (PublishedArticle, publish)
import Shared.Domain.Error (DomainError)

valid :: UTCTime -> ReadyToPublish -> Either DomainError PublishedArticle
valid = publish
