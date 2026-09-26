module Domain.Article.DraftSpec (run) where

import Data.Either (isLeft)
import Data.Set qualified as Set
import Domain.Article.Common
import Domain.Article.Draft
import Shared.Domain.Excerpt (newExcerpt)
import TestSupport

run :: IO ()
run = do
    value <- right identifier
    empty <- right emptyContent
    initial <- right (newUnvalidatedDraft value (timestamp 0) empty)
    none <- right (confirmAvailableImageReferences Set.empty Set.empty)
    check
        "title-only draft"
        ( draftBodyText (draftContent initial).body == ""
            && (draftContent initial).slug == Nothing
            && draftIdentifier initial == value
        )
    check "incomplete rejected" (isLeft (proofread (timestamp 1) none initial))
    content <- right (newDraftContent extractImages input)
    draft <- right (amendDraft (timestamp 1) content initial)
    check
        "amend preserves identity and creation"
        ( draftIdentifier draft == value
            && (draftTimeline draft).createdAt == timestamp 0
            && (draftTimeline draft).updatedAt == timestamp 1
        )
    check "amend stale timestamp" (isLeft (amendDraft (timestamp 0) content draft))
    available <- right confirmed
    proof <- right (proofread (timestamp 2) available draft)
    check
        "proofreading preserves identity and content"
        ( draftIdentifier proof == value
            && (draftTimeline proof).updatedAt == timestamp 2
            && contentText (proofreadedContent proof).body == input.body
        )
    check "proofreading stale timestamp" (isLeft (proofread (timestamp 0) available draft))
    excerpt <- right (newExcerpt "Summary")
    ready <- right (prepareToPublish (timestamp 3) excerpt proof)
    check
        "ready has excerpt"
        ( (publicationContent ready).excerpt == excerpt
            && draftIdentifier ready == value
            && (draftTimeline ready).updatedAt == timestamp 3
        )
    check "prepare stale timestamp" (isLeft (prepareToPublish (timestamp 1) excerpt proof))
    edited <- right (newExcerpt "Edited summary")
    revised <- right (reviseExcerpt (timestamp 4) edited ready)
    check
        "excerpt edit preserves preparation"
        ( (publicationContent revised).excerpt == edited
            && (publicationContent revised).body == (publicationContent ready).body
            && (draftTimeline revised).createdAt == timestamp 0
            && (draftTimeline revised).updatedAt == timestamp 4
        )
    check "excerpt edit stale timestamp" (isLeft (reviseExcerpt (timestamp 2) edited ready))
    proofAmended <- right (amendDraft (timestamp 4) content proof)
    readyAmended <- right (amendDraft (timestamp 4) content ready)
    check
        "identical save invalidates preparation"
        ( draftContent proofAmended == content
            && draftContent readyAmended == content
            && (draftTimeline readyAmended).updatedAt == timestamp 4
        )
    cleared <- right (amendDraft (timestamp 5) empty revised)
    check
        "full replacement clears body slug tags images"
        (draftContent cleared == empty && Set.null (draftContent cleared).images)
    noImages <-
        right
            ( newDraftContent
                extractImages
                (DraftInput input.title "No images" input.slug [])
            )
    noImagesDraft <- right (newUnvalidatedDraft value (timestamp 0) noImages)
    noImagesProof <- right (proofread (timestamp 1) none noImagesDraft)
    check "no images and tags required" (Set.null (proofreadedContent noImagesProof).images)
