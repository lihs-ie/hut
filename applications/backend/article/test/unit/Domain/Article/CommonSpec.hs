module Domain.Article.CommonSpec (run) where

import Data.Either (isLeft, isRight)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Domain.Article.Common
import Shared.Domain.Date (newTimeline)
import Shared.Domain.Error (createServiceUnavailable)
import Shared.Domain.Excerpt (excerptText, newExcerpt)
import Shared.Domain.Slug (newSlug, slugText)
import TestSupport

run :: IO ()
run = do
    check
        "title boundaries"
        ( all (isLeft . newTitle) ["", "  ", Text.replicate 101 "a"]
            && all (isRight . newTitle) ["a", Text.replicate 100 "a"]
        )
    check
        "content has no domain size limit"
        ( all (isLeft . newContent) ["", " \n\t"]
            && isRight (newContent (Text.replicate 100001 "a"))
        )
    title <- right (newTitle "Title")
    content <- right (newContent "Body")
    check
        "text preserved"
        ( titleText title == "Title"
            && contentText content == "Body"
            && draftBodyText (newDraftBody "  ") == "  "
            && draftBodyText (newDraftBody "") == ""
        )
    value <- right identifier
    reference <- right image
    check
        "identifier round trip"
        ( newArticleIdentifier (articleIdentifierText value) == Right value
            && isLeft (newArticleIdentifier "haskell-syntax")
        )
    check
        "reference round trip"
        ( newImageReference (imageReferenceText reference) == Right reference
            && isLeft (newImageReference "invalid")
        )
    check
        "slug boundaries"
        ( all (isRight . newSlug) ["a", "0", "a-z-09", "haskell-syntax"]
            && all
                (isLeft . newSlug)
                ["", "A", "-a", "a-", "a--b", "a/b", "a_b", "a b", "１２３"]
        )
    slug <- right (newSlug "haskell-syntax")
    check "slug text" (slugText slug == "haskell-syntax")
    check
        "excerpt boundaries"
        ( all (isLeft . newExcerpt) ["", " \n", Text.replicate 201 "a"]
            && all (isRight . newExcerpt) ["a", Text.replicate 200 "a"]
        )
    excerpt <- right (newExcerpt "Summary")
    check "excerpt text" (excerptText excerpt == "Summary")
    draft <- right (newDraftContent extractImages input)
    check
        "saved fields are validated and derived"
        ( titleText draft.title == input.title
            && draftBodyText draft.body == input.body
            && draft.slug == Just slug
            && length draft.tags == 1
            && draft.images == Set.singleton reference
        )
    deduplicated <-
        right
            ( newDraftContent
                extractImages
                ( DraftInput
                    input.title
                    input.body
                    input.slug
                    ["01ARZ3NDEKTSV4RRFFQ69G5FAY", "01ARZ3NDEKTSV4RRFFQ69G5FAY"]
                )
            )
    check "tags deduplicated" (length deduplicated.tags == 1)
    check
        "draft input validation"
        ( all
            (isLeft . newDraftContent extractImages)
            [ DraftInput "" input.body input.slug input.tags
            , DraftInput input.title input.body (Just "BAD") input.tags
            , DraftInput input.title input.body input.slug [""]
            ]
        )
    let extractionFailure = createServiceUnavailable "Parser" "unavailable"
    check
        "extraction error propagated"
        (newDraftContent (const (Left extractionFailure)) input == Left extractionFailure)
    empty <- right emptyContent
    available <- right confirmed
    none <- right (confirmAvailableImageReferences Set.empty Set.empty)
    check
        "incomplete draft cannot complete proofreading"
        (isLeft (proofreadContent none empty))
    missingSlug <-
        right
            ( newDraftContent
                extractImages
                (DraftInput input.title input.body Nothing input.tags)
            )
    check "slug required" (isLeft (proofreadContent available missingSlug))
    check
        "confirmation must cover exactly requested images"
        ( isLeft (confirmAvailableImageReferences (Set.singleton reference) Set.empty)
            && isLeft (confirmAvailableImageReferences Set.empty (Set.singleton reference))
        )
    different <- right otherImage
    wrong <-
        right
            ( confirmAvailableImageReferences
                (Set.singleton different)
                (Set.singleton different)
            )
    check
        "confirmation for different draft rejected"
        (isLeft (proofreadContent wrong draft))
    proof <- right (proofreadContent available draft)
    let publication = newPublicationContent excerpt proof
    check
        "proofread and publication preserve all fields"
        ( titleText proof.title == input.title
            && contentText proof.body == input.body
            && proof.slug == slug
            && proof.tags == draft.tags
            && proof.images == draft.images
            && publication.title == proof.title
            && publication.body == proof.body
            && publication.slug == proof.slug
            && publication.tags == proof.tags
            && publication.images == proof.images
            && publication.excerpt == excerpt
        )
    changedExcerpt <- right (newExcerpt "Edited")
    let changed = replaceExcerpt changedExcerpt publication
    check
        "excerpt replacement preserves remaining fields"
        ( changed.excerpt == changedExcerpt
            && changed.title == publication.title
            && changed.body == publication.body
            && changed.slug == publication.slug
            && changed.tags == publication.tags
            && changed.images == publication.images
        )
    timeline <- right (newTimeline (timestamp 0) (timestamp 1))
    check "time cannot regress" (isLeft (amendTimeline (timestamp 0) timeline))
    check "same instant allowed" (amendTimeline (timestamp 1) timeline == Right timeline)
    advanced <- right (amendTimeline (timestamp 2) timeline)
    check "creation preserved" (advanced.createdAt == timestamp 0 && advanced.updatedAt == timestamp 2)
