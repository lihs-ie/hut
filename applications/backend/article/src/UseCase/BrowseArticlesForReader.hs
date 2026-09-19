module UseCase.BrowseArticlesForReader (
    BrowseArticlesForReaderPayload (..),
    BrowseArticlesForReaderCommand,
    BrowseArticlesForReaderResult (..),
    Dependencies (..),
    browseArticlesForReader,
) where

import Domain.Article.Published (PublishedArticle)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Pager (Pager)
import Shared.UseCase.Command (Command (..))
import UseCase.Reading
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

data BrowseArticlesForReaderPayload = BrowseArticlesForReaderPayload
    { current :: Int
    , items :: Maybe Int
    }
    deriving stock (Show, Eq)
type BrowseArticlesForReaderCommand = Command BrowseArticlesForReaderPayload
data BrowseArticlesForReaderResult = BrowseArticlesForReaderResult
    { articles :: [PublishedArticle]
    , pager :: Pager
    , events :: Events (ArticleEventsFor 'Result.BrowseArticlesForReader)
    }

-- Only published articles, ordered by (publishedAt DESC, identifier DESC).
newtype Dependencies m = Dependencies
    { browseArticles :: ReadPage m PublishedArticle
    }

browseArticlesForReader ::
    (Monad m) =>
    Dependencies m ->
    BrowseArticlesForReaderCommand ->
    m (Either DomainError BrowseArticlesForReaderResult)
browseArticlesForReader dependencies command = case newPageRequest command.payload.current command.payload.items of
    Left err -> pure (Left err)
    Right request -> do
        found <- dependencies.browseArticles request
        pure $ do
            (total, articles) <- found
            pager <- pageResult request total articles
            pure (BrowseArticlesForReaderResult articles pager (Events []))
