module InvalidDiscardEvent where

import Domain.Article.Common (ArticleIdentifier)
import Domain.Article.Event (ArticleTakenDown)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import UseCase.Result (ArticleEventsFor, ArticleUseCase (DiscardArticle))

invalid :: ArticleIdentifier -> Events (ArticleEventsFor 'DiscardArticle)
invalid payload = Events [Here (DomainEvent payload :: ArticleTakenDown)]
