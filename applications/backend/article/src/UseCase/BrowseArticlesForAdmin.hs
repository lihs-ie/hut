module UseCase.BrowseArticlesForAdmin (
    BrowseArticlesForAdminPayload (..),
    BrowseArticlesForAdminCommand,
    BrowseArticlesForAdminResult (..),
    Dependencies (..),
    browseArticlesForAdmin,
) where

import Domain.Article (Article)
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
newtype Dependencies m = Dependencies
    { browseArticles :: ArticleFilter -> ReadPage m Article
    }

browseArticlesForAdmin ::
    (Monad m) =>
    Dependencies m ->
    BrowseArticlesForAdminCommand ->
    m (Either DomainError BrowseArticlesForAdminResult)
browseArticlesForAdmin dependencies command = case newPageRequest command.payload.current command.payload.items of
    Left err -> pure (Left err)
    Right request -> do
        found <- dependencies.browseArticles command.payload.status request
        pure $ do
            (total, articles) <- found
            pager <- pageResult request total articles
            if all (matchesFilter command.payload.status) articles
                then Right (BrowseArticlesForAdminResult articles pager (Events []))
                else Left (createUnexpectedError "Article" "page contains articles outside requested state")
