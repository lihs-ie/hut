module Domain.Article.LifecycleSpec (run) where

import Data.Either (isLeft)
import Domain.Article qualified as Article
import Domain.Article.Common
import Domain.Article.Draft
import Domain.Article.Private (resumePublication, takeDown)
import Domain.Article.Published (publish)
import Shared.Domain.Excerpt (newExcerpt)
import TestSupport

run :: IO ()
run = do
    draft <- right start
    available <- right confirmed
    proof <- right (proofread (timestamp 1) available draft)
    excerpt <- right (newExcerpt "Summary")
    ready <- right (prepareToPublish (timestamp 2) excerpt proof)
    published <- right (publish (timestamp 3) ready)
    check
        "publication preserves content and creation"
        ( published.publication == publicationContent ready
            && published.publishedAt == timestamp 3
            && published.timeline.createdAt == timestamp 0
            && published.timeline.updatedAt == timestamp 3
        )
    check "publish rejects stale time" (isLeft (publish (timestamp 1) ready))
    private <- right (takeDown (timestamp 4) published)
    check
        "take down preserves content and publication date"
        ( private.publication == published.publication
            && private.identifier == published.identifier
            && private.publishedAt == timestamp 3
            && private.timeline.createdAt == timestamp 0
            && private.timeline.updatedAt == timestamp 4
        )
    check "take down rejects stale time" (isLeft (takeDown (timestamp 2) published))
    resumed <- right (resumePublication (timestamp 5) private)
    check
        "resume keeps excerpt and updates timestamp"
        ( publicationContent resumed == private.publication
            && (draftTimeline resumed).createdAt == timestamp 0
            && (draftTimeline resumed).updatedAt == timestamp 5
        )
    republished <- right (publish (timestamp 6) resumed)
    check
        "republish renews publication date"
        (republished.publishedAt == timestamp 6 && republished.publication == private.publication)
    check "resume rejects stale time" (isLeft (resumePublication (timestamp 3) private))
    replacement <-
        right
            ( newDraftContent
                extractImages
                (DraftInput input.title input.body (Just "changed-slug") input.tags)
            )
    edited <- right (amendDraft (timestamp 6) replacement resumed)
    check "previously published slug can change" (draftContent edited == replacement)
    value <- right identifier
    check
        "all aggregate states retain identity"
        ( all
            ((== value) . Article.articleIdentifier)
            [ Article.Unvalidated draft
            , Article.Proofreaded proof
            , Article.Ready ready
            , Article.Published published
            , Article.Private private
            ]
        )
