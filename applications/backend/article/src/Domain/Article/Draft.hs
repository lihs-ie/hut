{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Domain.Article.Draft (
    DraftPhase (..),
    Draft,
    UnvalidatedDraft,
    ProofreadedDraft,
    ReadyToPublish,
    newUnvalidatedDraft,
    newUnvalidatedDraftWithTimeline,
    newProofreadedDraft,
    amendDraft,
    proofread,
    prepareToPublish,
    reviseExcerpt,
    newReadyToPublish,
    draftIdentifier,
    draftTimeline,
    draftContent,
    proofreadedContent,
    publicationContent,
) where

import Data.Time (UTCTime)
import Domain.Article.Common
import Shared.Domain.Date (Timeline, newTimeline)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Excerpt (Excerpt)

data DraftPhase = Unvalidated | Proofreaded | Ready

data Draft (phase :: DraftPhase) where
    UnvalidatedDraft ::
        ArticleIdentifier -> DraftContent -> Timeline -> Draft 'Unvalidated
    ProofreadedDraft ::
        ArticleIdentifier -> ProofreadedContent -> Timeline -> Draft 'Proofreaded
    ReadyToPublish ::
        ArticleIdentifier -> PublicationContent -> Timeline -> Draft 'Ready

deriving stock instance Show (Draft phase)
deriving stock instance Eq (Draft phase)

type UnvalidatedDraft = Draft 'Unvalidated
type ProofreadedDraft = Draft 'Proofreaded
type ReadyToPublish = Draft 'Ready

newUnvalidatedDraft ::
    ArticleIdentifier -> UTCTime -> DraftContent -> Either DomainError UnvalidatedDraft
newUnvalidatedDraft identifier timestamp content = do
    timeline <- newTimeline timestamp timestamp
    pure (UnvalidatedDraft identifier content timeline)

newUnvalidatedDraftWithTimeline ::
    ArticleIdentifier -> DraftContent -> Timeline -> UnvalidatedDraft
newUnvalidatedDraftWithTimeline = UnvalidatedDraft

newProofreadedDraft ::
    ArticleIdentifier -> ProofreadedContent -> Timeline -> ProofreadedDraft
newProofreadedDraft = ProofreadedDraft

amendDraft ::
    UTCTime -> DraftContent -> Draft phase -> Either DomainError UnvalidatedDraft
amendDraft timestamp content draft = do
    timeline <- amendTimeline timestamp (draftTimeline draft)
    pure (UnvalidatedDraft (draftIdentifier draft) content timeline)

proofread ::
    UTCTime ->
    AvailableImageReferences ->
    UnvalidatedDraft ->
    Either DomainError ProofreadedDraft
proofread timestamp available (UnvalidatedDraft identifier content previous) = do
    timeline <- amendTimeline timestamp previous
    validated <- proofreadContent available content
    pure (ProofreadedDraft identifier validated timeline)

prepareToPublish ::
    UTCTime -> Excerpt -> ProofreadedDraft -> Either DomainError ReadyToPublish
prepareToPublish timestamp excerpt (ProofreadedDraft identifier content previous) = do
    timeline <- amendTimeline timestamp previous
    pure (ReadyToPublish identifier (newPublicationContent excerpt content) timeline)

reviseExcerpt ::
    UTCTime -> Excerpt -> ReadyToPublish -> Either DomainError ReadyToPublish
reviseExcerpt timestamp excerpt (ReadyToPublish identifier content previous) = do
    timeline <- amendTimeline timestamp previous
    pure (ReadyToPublish identifier (replaceExcerpt excerpt content) timeline)

-- Reconstitutes a ready draft from already validated publication content.
newReadyToPublish :: ArticleIdentifier -> PublicationContent -> Timeline -> ReadyToPublish
newReadyToPublish = ReadyToPublish

draftIdentifier :: Draft phase -> ArticleIdentifier
draftIdentifier (UnvalidatedDraft identifier _ _) = identifier
draftIdentifier (ProofreadedDraft identifier _ _) = identifier
draftIdentifier (ReadyToPublish identifier _ _) = identifier

draftTimeline :: Draft phase -> Timeline
draftTimeline (UnvalidatedDraft _ _ timeline) = timeline
draftTimeline (ProofreadedDraft _ _ timeline) = timeline
draftTimeline (ReadyToPublish _ _ timeline) = timeline

draftContent :: UnvalidatedDraft -> DraftContent
draftContent (UnvalidatedDraft _ content _) = content

proofreadedContent :: ProofreadedDraft -> ProofreadedContent
proofreadedContent (ProofreadedDraft _ content _) = content

publicationContent :: ReadyToPublish -> PublicationContent
publicationContent (ReadyToPublish _ content _) = content
