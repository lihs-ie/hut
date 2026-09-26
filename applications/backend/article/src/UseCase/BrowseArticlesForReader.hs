module UseCase.BrowseArticlesForReader (
    BrowseArticlesForReaderPayload (..),
    BrowseArticlesForReaderCommand,
    BrowseArticlesForReaderResult (..),
    Dependencies (..),
    browseArticlesForReader,
) where

import Domain.Article
import Data.Text (Text)
import Domain.Article.Published (PublishedArticle)
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, fromEither, runTransaction)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Pager (Pager)
import Shared.Domain.Tag (TagIdentifier)
import Shared.UseCase.Command (Command (..))
import UseCase.Reading
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

data BrowseArticlesForReaderPayload = BrowseArticlesForReaderPayload
    { current :: Int
    , items :: Maybe Int
    , keyword :: Maybe Text
    , tags :: [TagIdentifier]
    }
    deriving stock (Show, Eq)
type BrowseArticlesForReaderCommand = Command BrowseArticlesForReaderPayload
data BrowseArticlesForReaderResult = BrowseArticlesForReaderResult
    { articles :: [PublishedArticle]
    , pager :: Pager
    , events :: Events (ArticleEventsFor 'Result.BrowseArticlesForReader)
    }

-- Only published articles, ordered by (publishedAt DESC, identifier DESC).
data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , searchArticles :: SearchPublishedArticles (Transaction context m)
    }

browseArticlesForReader ::
    (Monad m) =>
    Dependencies context m ->
    BrowseArticlesForReaderCommand ->
    m (Either DomainError BrowseArticlesForReaderResult)
browseArticlesForReader dependencies command = runTransaction dependencies.transactionManager $ do
    request <- fromEither $
        newReaderCriteria
            command.payload.current
            command.payload.items
            command.payload.keyword
            command.payload.tags
    (total, articles) <- dependencies.searchArticles request
    pager <- fromEither (pageResult request total articles)
    pure (BrowseArticlesForReaderResult articles pager (Events []))
