module InvalidPublishEvent where

import Domain.Article.Common (ArticleIdentifier)
import Domain.Article.Event (ArticleTakenDown)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import UseCase.Result (ArticleEventsFor, ArticleUseCase (Publish))

invalid :: ArticleIdentifier -> Events (ArticleEventsFor 'Publish)
invalid payload = Events [Here (DomainEvent payload :: ArticleTakenDown)]
