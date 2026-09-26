module UseCase.ReadArticle (
    ReadArticlePayload (..),
    ReadArticleCommand,
    ReadArticleResult (..),
    Dependencies (..),
    readArticle,
) where

import Data.Text (Text)
import Domain.Article
import Domain.Article.Published (PublishedArticle)
import Shared.Domain.Common.Transaction (Transaction, TransactionManager, abort, fromEither, runTransaction)
import Shared.Domain.Error (DomainError, createAggregateNotFound, createUnexpectedError)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Slug (newSlug, slugText)
import Shared.UseCase.Command (Command (..))
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype ReadArticlePayload = ReadArticlePayload {slug :: Text}
    deriving stock (Show, Eq)
type ReadArticleCommand = Command ReadArticlePayload
data ReadArticleResult = ReadArticleResult
    { article :: PublishedArticle
    , events :: Events (ArticleEventsFor 'Result.ReadArticle)
    }
data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticleBySlug :: FindArticleBySlug (Transaction context m)
    }

readArticle :: (Monad m) => Dependencies context m -> ReadArticleCommand -> m (Either DomainError ReadArticleResult)
readArticle dependencies command = runTransaction dependencies.transactionManager $ do
    slug <- fromEither (newSlug command.payload.slug)
    found <- dependencies.findArticleBySlug slug
    case found of
        Just (Published article)
            | article.publication.slug == slug -> pure (ReadArticleResult article (Events []))
            | otherwise -> abort (createUnexpectedError "Article" "loaded slug does not match request")
        _ -> abort (createAggregateNotFound "Article" (slugText slug))
