module InvalidViewArticleForAdminEvent where
import Domain.Article.Common (ArticleIdentifier)
import Domain.Article.Event (ArticlePublished)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import UseCase.Result (ArticleEventsFor, ArticleUseCase (ViewArticleForAdmin))
invalid :: ArticleIdentifier -> Events (ArticleEventsFor 'ViewArticleForAdmin)
invalid value = Events [Here (DomainEvent value :: ArticlePublished)]
