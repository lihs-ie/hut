module Domain.Article.Event (
    ArticleEventKind (..),
    ImageReferences (..),
    ArticleDraftStarted,
    ArticleDraftAmended,
    draftImageReferences,
    ArticleProofreaded,
    ArticleReadyToPublish,
    ArticlePublished,
    ArticleTakenDown,
    ArticleDiscarded,
) where

import Data.Set (Set)
import Domain.Article.Common (ArticleIdentifier, ImageReference)
import Domain.Article.Draft (UnvalidatedDraft, draftContent, draftIdentifier)
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

type ArticleProofreaded = DomainEvent 'Proofreaded ArticleIdentifier
type ArticleReadyToPublish = DomainEvent 'ReadyToPublish ArticleIdentifier
type ArticlePublished = DomainEvent 'Published ArticleIdentifier
type ArticleTakenDown = DomainEvent 'TakenDown ArticleIdentifier
type ArticleDiscarded = DomainEvent 'Discarded ArticleIdentifier
