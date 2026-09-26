module InvalidTakeDownEvent where

import Domain.Article.Common (ArticleIdentifier)
import Domain.Article.Event (ArticlePublished)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import UseCase.Result (ArticleEventsFor, ArticleUseCase (TakeDown))

invalid :: ArticleIdentifier -> Events (ArticleEventsFor 'TakeDown)
invalid payload = Events [Here (DomainEvent payload :: ArticlePublished)]
