module InvalidReadArticleEvent where
import Domain.Article.Common (ArticleIdentifier)
import Domain.Article.Event (ArticlePublished)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import UseCase.Result (ArticleEventsFor, ArticleUseCase (ReadArticle))
invalid :: ArticleIdentifier -> Events (ArticleEventsFor 'ReadArticle)
invalid value = Events [Here (DomainEvent value :: ArticlePublished)]
