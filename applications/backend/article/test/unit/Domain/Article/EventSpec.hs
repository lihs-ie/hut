module Domain.Article.EventSpec (run) where

import Domain.Article.Draft
import Domain.Article.Event
import Shared.Domain.Event (DomainEvent (..))
import TestSupport

run :: IO ()
run = do
    draft <- right start
    available <- right confirmed
    let references = draftImageReferences draft
    check "draft event identifies its aggregate" (references.article == draftIdentifier draft)
    check "draft event includes all image references" (references.images == (draftContent draft).images)
    proof <- right (proofread (timestamp 1) available draft)
    let event :: ArticleProofreaded
        event = DomainEvent (draftIdentifier proof)
    check "proofread event payload is only its aggregate identifier" (event.payload == draftIdentifier proof)
