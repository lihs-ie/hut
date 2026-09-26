module UseCase.BrowseArticlesForAdmin (
    BrowseArticlesForAdminPayload (..),
    BrowseArticlesForAdminCommand,
    BrowseArticlesForAdminResult (..),
    Dependencies (..),
    browseArticlesForAdmin,
) where

import Domain.Article
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, abort, fromEither, runTransaction)
import Shared.Domain.Error (DomainError, createUnexpectedError)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Pager (Pager)
import Shared.UseCase.Command (Command (..))
import UseCase.Reading
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

data BrowseArticlesForAdminPayload = BrowseArticlesForAdminPayload
    { current :: Int
    , items :: Maybe Int
    , status :: ArticleFilter
    }
    deriving stock (Show, Eq)
type BrowseArticlesForAdminCommand = Command BrowseArticlesForAdminPayload
data BrowseArticlesForAdminResult = BrowseArticlesForAdminResult
    { articles :: [Article]
    , pager :: Pager
    , events :: Events (ArticleEventsFor 'Result.BrowseArticlesForAdmin)
    }

-- Apply the state filter before counting/paging; order by (updatedAt DESC, identifier DESC).
data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , searchArticles :: SearchArticles (Transaction context m)
    }

browseArticlesForAdmin ::
    (Monad m) =>
    Dependencies context m ->
    BrowseArticlesForAdminCommand ->
    m (Either DomainError BrowseArticlesForAdminResult)
browseArticlesForAdmin dependencies command = runTransaction dependencies.transactionManager $ do
    request <- fromEither (newCriteria command.payload.status command.payload.current command.payload.items)
    (total, articles) <- dependencies.searchArticles request
    pager <- fromEither (pageResult request total articles)
    if all (matchesFilter command.payload.status) articles
        then pure (BrowseArticlesForAdminResult articles pager (Events []))
        else abort (createUnexpectedError "Article" "page contains articles outside requested state")
