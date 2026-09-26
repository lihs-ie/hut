module InvalidBrowseArticlesForAdminEvent where
import Domain.Article.Common (ArticleIdentifier)
import Domain.Article.Event (ArticlePublished)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import UseCase.Result (ArticleEventsFor, ArticleUseCase (BrowseArticlesForAdmin))
invalid :: ArticleIdentifier -> Events (ArticleEventsFor 'BrowseArticlesForAdmin)
invalid value = Events [Here (DomainEvent value :: ArticlePublished)]
