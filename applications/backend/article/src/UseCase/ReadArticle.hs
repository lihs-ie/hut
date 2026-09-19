module UseCase.ReadArticle (
    ReadArticlePayload (..),
    ReadArticleCommand,
    ReadArticleResult (..),
    Dependencies (..),
    readArticle,
) where

import Data.Text (Text)
import Domain.Article (Article (..))
import Domain.Article.Published (PublishedArticle)
import Shared.Domain.Error (DomainError, createAggregateNotFound, createUnexpectedError)
import Shared.Domain.Event (Events (..))
import Shared.Domain.Slug (Slug, newSlug, slugText)
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
newtype Dependencies m = Dependencies
    { findArticleBySlug :: Slug -> m (Either DomainError (Maybe Article))
    }

readArticle ::
    (Monad m) =>
    Dependencies m ->
    ReadArticleCommand ->
    m (Either DomainError ReadArticleResult)
readArticle dependencies command = case newSlug command.payload.slug of
    Left err -> pure (Left err)
    Right slug -> do
        found <- dependencies.findArticleBySlug slug
        pure $ case found of
            Left err -> Left err
            Right (Just (Published article))
                | article.publication.slug == slug -> Right (ReadArticleResult article (Events []))
                | otherwise -> Left (createUnexpectedError "Article" "loaded slug does not match request")
            Right _ -> Left (createAggregateNotFound "Article" (slugText slug))
