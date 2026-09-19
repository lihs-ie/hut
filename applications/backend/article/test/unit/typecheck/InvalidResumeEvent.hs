module InvalidResumeEvent where

import Domain.Article.Common (ArticleIdentifier)
import Domain.Article.Event (ArticleReadyToPublish)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import UseCase.Result (ArticleEventsFor, ArticleUseCase (ResumePublication))

invalid :: ArticleIdentifier -> Events (ArticleEventsFor 'ResumePublication)
invalid payload = Events [Here (DomainEvent payload :: ArticleReadyToPublish)]
