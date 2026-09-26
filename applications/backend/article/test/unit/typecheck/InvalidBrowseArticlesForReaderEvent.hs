module InvalidBrowseArticlesForReaderEvent where
import Domain.Article.Common (ArticleIdentifier)
import Domain.Article.Event (ArticlePublished)
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import UseCase.Result (ArticleEventsFor, ArticleUseCase (BrowseArticlesForReader))
invalid :: ArticleIdentifier -> Events (ArticleEventsFor 'BrowseArticlesForReader)
invalid value = Events [Here (DomainEvent value :: ArticlePublished)]
