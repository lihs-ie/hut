module InvalidCheckSlugAvailabilityEvent where
import Domain.Article.Common (ArticleIdentifier)
import Domain.Article.Event (ArticlePublished)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import UseCase.Result (ArticleEventsFor, ArticleUseCase (CheckSlugAvailability))
invalid :: ArticleIdentifier -> Events (ArticleEventsFor 'CheckSlugAvailability)
invalid value = Events [Here (DomainEvent value :: ArticlePublished)]
