module UseCase.ViewArticleForAdmin (
    ViewArticleForAdminPayload (..),
    ViewArticleForAdminCommand,
    ViewArticleForAdminResult (..),
    Dependencies (..),
    viewArticleForAdmin,
) where

import Domain.Article (Article (..), articleIdentifier)
import Domain.Article.Common (ArticleIdentifier, articleIdentifierText)

import Shared.Domain.Error (DomainError, createAggregateNotFound, createUnexpectedError)
import Shared.Domain.Event (Events (..))
import Shared.UseCase.Command (Command (..))
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype ViewArticleForAdminPayload = ViewArticleForAdminPayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)
type ViewArticleForAdminCommand = Command ViewArticleForAdminPayload
data ViewArticleForAdminResult = ViewArticleForAdminResult
    { article :: Article
    , events :: Events (ArticleEventsFor 'Result.ViewArticleForAdmin)
    }
newtype Dependencies m = Dependencies
    { findArticle :: ArticleIdentifier -> m (Either DomainError (Maybe Article))
    }

viewArticleForAdmin ::
    (Monad m) =>
    Dependencies m ->
    ViewArticleForAdminCommand ->
    m (Either DomainError ViewArticleForAdminResult)
viewArticleForAdmin dependencies command = do
    found <- dependencies.findArticle command.payload.article
    pure $ case found of
        Left err -> Left err
        Right Nothing -> Left (createAggregateNotFound "Article" (articleIdentifierText command.payload.article))
        Right (Just article)
            | articleIdentifier article == command.payload.article ->
                Right (ViewArticleForAdminResult article (Events []))
            | otherwise -> Left (createUnexpectedError "Article" "loaded identity does not match request")
