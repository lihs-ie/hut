module Domain.Article.Event (
    ArticleEventKind (..),
    ImageReferences (..),
    ArticleDraftStarted,
    ArticleDraftAmended,
    draftImageReferences,
    ProofreadedArticleContent (..),
    ArticleProofreaded,
    ArticleReadyToPublish,
    ArticlePublished,
    ArticleTakenDown,
    ArticleDiscarded,
    proofreadedArticleContent,
) where

import Data.Set (Set)
import Domain.Article.Common (ArticleIdentifier, Content, ImageReference, Title)
import Domain.Article.Draft (ProofreadedDraft, UnvalidatedDraft, draftContent, draftIdentifier, proofreadedContent)
import Shared.Domain.Event (DomainEvent)

data ArticleEventKind = DraftStarted | DraftAmended | Proofreaded | ReadyToPublish | Published | TakenDown | Discarded

data ImageReferences = ImageReferences
    { article :: ArticleIdentifier
    , images :: Set ImageReference
    }
    deriving stock (Show, Eq)

type ArticleDraftStarted = DomainEvent 'DraftStarted ImageReferences
type ArticleDraftAmended = DomainEvent 'DraftAmended ImageReferences

draftImageReferences :: UnvalidatedDraft -> ImageReferences
draftImageReferences draft =
    ImageReferences (draftIdentifier draft) (draftContent draft).images

data ProofreadedArticleContent = ProofreadedArticleContent
    { article :: ArticleIdentifier
    , title :: Title
    , body :: Content
    }
    deriving stock (Show, Eq)

type ArticleProofreaded = DomainEvent 'Proofreaded ProofreadedArticleContent
type ArticleReadyToPublish = DomainEvent 'ReadyToPublish ArticleIdentifier
type ArticlePublished = DomainEvent 'Published ArticleIdentifier
type ArticleTakenDown = DomainEvent 'TakenDown ArticleIdentifier
type ArticleDiscarded = DomainEvent 'Discarded ArticleIdentifier

proofreadedArticleContent :: ProofreadedDraft -> ProofreadedArticleContent
proofreadedArticleContent draft =
    ProofreadedArticleContent
        (draftIdentifier draft)
        (proofreadedContent draft).title
        (proofreadedContent draft).body
