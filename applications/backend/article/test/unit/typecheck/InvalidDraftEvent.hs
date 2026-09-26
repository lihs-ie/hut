module InvalidDraftEvent where

import Domain.Article.Event (ArticleDraftAmended, ImageReferences)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import UseCase.Result (ArticleEventsFor, ArticleUseCase (JotDown))

invalid :: ImageReferences -> Events (ArticleEventsFor 'JotDown)
invalid payload = Events [Here (DomainEvent payload :: ArticleDraftAmended)]
