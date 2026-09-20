module UseCase.ViewArticleForAdmin (
    ViewArticleForAdminPayload (..),
    ViewArticleForAdminCommand,
    ViewArticleForAdminResult (..),
    Dependencies (..),
    viewArticleForAdmin,
) where

import Domain.Article

import Shared.Domain.Common.Transaction (Transaction, TransactionManager, runTransaction)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Event (Events (..))
import Shared.UseCase.Command (Command (..))
import UseCase.Helper
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype ViewArticleForAdminPayload = ViewArticleForAdminPayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)
type ViewArticleForAdminCommand = Command ViewArticleForAdminPayload
data ViewArticleForAdminResult = ViewArticleForAdminResult
    { article :: Article
    , events :: Events (ArticleEventsFor 'Result.ViewArticleForAdmin)
    }
data Dependencies context m = Dependencies
    { transactionManager :: TransactionManager context m
    , findArticle :: FindArticle (Transaction context m)
    }

viewArticleForAdmin ::
    (Monad m) =>
    Dependencies context m ->
    ViewArticleForAdminCommand ->
    m (Either DomainError ViewArticleForAdminResult)
viewArticleForAdmin dependencies command = runTransaction dependencies.transactionManager $ do
    article <- requireArticle dependencies.findArticle command.payload.article
    pure (ViewArticleForAdminResult article (Events []))
