module UseCase.Proofread (
    ProofreadPayload (..),
    ProofreadCommand,
    ProofreadResult (..),
    Dependencies (..),
    proofread,
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Domain.Article (Article (..), articleIdentifier)
import Domain.Article.Common (
    ArticleIdentifier,
    ImageReference,
    articleIdentifierText,
    confirmAvailableImageReferences,
 )
import Domain.Article.Draft qualified as Draft
import Domain.Article.Event (proofreadedArticleContent)
import Shared.Domain.Error (
    DomainError,
    createAggregateNotFound,
    createOperationNotAllowed,
    createUnexpectedError,
 )
import Shared.Domain.Event (DomainEvent (..), Events (..), OneOf (..))
import Shared.UseCase.Command (Command (..))
import UseCase.Persistence (LoadForProofreading, LoadedForProofreading (..), commandContext)
import UseCase.Result (ArticleEventsFor)
import UseCase.Result qualified as Result

newtype ProofreadPayload = ProofreadPayload {article :: ArticleIdentifier}
    deriving stock (Show, Eq)

type ProofreadCommand = Command ProofreadPayload

data ProofreadResult = ProofreadResult
    { article :: Draft.ProofreadedDraft
    , events :: Events (ArticleEventsFor 'Result.Proofread)
    }

data Dependencies m = Dependencies
    { loadArticle :: LoadForProofreading m
    , findAvailableImages :: Set ImageReference -> m (Either DomainError (Set ImageReference))
    }

proofread :: (Monad m) => Dependencies m -> ProofreadCommand -> m (Either DomainError ProofreadResult)
proofread dependencies command = do
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
            | otherwise -> case snapshot.article of
                Unvalidated draft -> do
                    let requested = (Draft.draftContent draft).images
                    availability <-
                        if Set.null requested
                            then pure (Right Set.empty)
                            else dependencies.findAvailableImages requested
                    case availability
                        >>= confirmAvailableImageReferences requested
                        >>= (\available -> Draft.proofread command.timestamp available draft) of
                        Left err -> pure (Left err)
                        Right article -> do
                            let events = Events [Here (DomainEvent (proofreadedArticleContent article))]
                            saved <- snapshot.saveProofreading (commandContext command) article events
                            pure (ProofreadResult article events <$ saved)
                _ ->
                    pure
                        ( Left
                            ( createOperationNotAllowed
                                "Proofread"
                                "only unvalidated drafts can be proofread"
                            )
                        )
