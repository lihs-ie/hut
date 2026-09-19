module UseCase.AmendDraft (
    AmendDraftPayload (..),
    AmendDraftCommand,
    AmendDraftResult (..),
    Dependencies (..),
    amendDraft,
) where

import Data.Text (Text)
import Domain.Article (Article (..), articleIdentifier)
import Domain.Article.Common (
    ArticleIdentifier,
    DraftInput (..),
    ExtractImageReferences,
    articleIdentifierText,
    newDraftContent,
 )
import Domain.Article.Draft qualified as Draft
import Domain.Article.Event (draftImageReferences)
import Shared.Domain.Error (
    DomainError,
    createAggregateNotFound,
    createOperationNotAllowed,
    createUnexpectedError,
 )
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..))
import UseCase.Persistence (LoadArticleForAmendment, LoadedArticle (..), commandContext)
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

data AmendDraftPayload = AmendDraftPayload
    { article :: ArticleIdentifier
    , title :: Text
    , body :: Text
    , slug :: Maybe Text
    , tags :: [Text]
    }
    deriving stock (Show, Eq)

type AmendDraftCommand = Command AmendDraftPayload

data AmendDraftResult = AmendDraftResult
    { article :: Draft.UnvalidatedDraft
    , events :: Events (ArticleEventsFor 'Result.AmendDraft)
    }

data Dependencies m = Dependencies
    { loadArticle :: LoadArticleForAmendment m
    , extractImageReferences :: ExtractImageReferences
    }

amendDraft ::
    (Monad m) => Dependencies m -> AmendDraftCommand -> m (Either DomainError AmendDraftResult)
amendDraft dependencies command = do
    loaded <- dependencies.loadArticle command.payload.article
    case loaded of
        Left err -> pure (Left err)
        Right Nothing ->
            pure
                ( Left
                    ( createAggregateNotFound
                        "Article"
                        (articleIdentifierText command.payload.article)
                    )
                )
        Right (Just snapshot)
            | articleIdentifier snapshot.article /= command.payload.article ->
                pure (Left (createUnexpectedError "Article" "loaded identity does not match request"))
            | otherwise -> case amend snapshot.article of
                Left err -> pure (Left err)
                Right article -> do
                    let events = Events [Here (DomainEvent (draftImageReferences article))]
                    saved <- snapshot.saveAmendment (commandContext command) article events
                    pure (AmendDraftResult article events <$ saved)
  where
    input =
        DraftInput
            command.payload.title
            command.payload.body
            command.payload.slug
            command.payload.tags
    amend article = case article of
        Unvalidated draft -> apply draft
        Proofreaded draft -> apply draft
        Ready draft -> apply draft
        Published _ -> Left notEditable
        Private _ -> Left notEditable
    apply :: Draft.Draft phase -> Either DomainError Draft.UnvalidatedDraft
    apply draft = do
        content <- newDraftContent dependencies.extractImageReferences input
        Draft.amendDraft command.timestamp content draft
    notEditable =
        createOperationNotAllowed
            "AmendDraft"
            "only drafts can be amended; take down and resume publication first"
