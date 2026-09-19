module UseCase.ResumePublication (
    ResumePublicationPayload (..),
    ResumePublicationCommand,
    ResumePublicationResult (..),
    Dependencies (..),
    resumePublication,
) where

import Domain.Article (Article (..), articleIdentifier)
import Domain.Article.Common (ArticleIdentifier, articleIdentifierText)
import Domain.Article.Draft (ReadyToPublish)
import Domain.Article.Private qualified as Private
import Shared.Domain.Error (DomainError, createAggregateNotFound, createOperationNotAllowed, createUnexpectedError)
import Shared.Domain.Event (Events (..))
import Shared.UseCase.Command (Command (..))
import UseCase.Persistence (LoadForResumption, LoadedForResumption (..), commandContext)
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype ResumePublicationPayload = ResumePublicationPayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)

type ResumePublicationCommand = Command ResumePublicationPayload

data ResumePublicationResult = ResumePublicationResult
    { article :: ReadyToPublish
    , events :: Events (ArticleEventsFor 'Result.ResumePublication)
    }

newtype Dependencies m = Dependencies {loadArticle :: LoadForResumption m}

resumePublication ::
    (Monad m) =>
    Dependencies m ->
    ResumePublicationCommand ->
    m (Either DomainError ResumePublicationResult)
resumePublication dependencies command = do
    loaded <- dependencies.loadArticle command.payload.article
    case loaded of
        Left err -> pure (Left err)
        Right Nothing ->
            pure (Left (createAggregateNotFound "Article" (articleIdentifierText command.payload.article)))
        Right (Just snapshot)
            | articleIdentifier snapshot.article /= command.payload.article ->
                pure (Left (createUnexpectedError "Article" "loaded identity does not match request"))
            | otherwise -> case snapshot.article of
                Private source ->
                    case Private.resumePublication command.timestamp source of
                        Left err -> pure (Left err)
                        Right article -> do
                            let events = Events []
                            saved <- snapshot.saveResumption (commandContext command) article events
                            pure (ResumePublicationResult article events <$ saved)
                _ ->
                    pure (Left (createOperationNotAllowed "ResumePublication" "only private articles can resume publication"))
