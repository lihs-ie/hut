module Domain.Article.EventSpec (run) where

import Domain.Article.Draft
import Domain.Article.Event
import TestSupport

run :: IO ()
run = do
    draft <- right start
    available <- right confirmed
    let references = draftImageReferences draft
    check "draft event identifies its aggregate" (references.article == draftIdentifier draft)
    check "draft event includes all image references" (references.images == (draftContent draft).images)
    proof <- right (proofread (timestamp 1) available draft)
    let snapshot = proofreadedArticleContent proof
    check "proofread event identifies its aggregate" (snapshot.article == draftIdentifier proof)
    check "proofread event snapshots title" (snapshot.title == (proofreadedContent proof).title)
    check "proofread event snapshots body" (snapshot.body == (proofreadedContent proof).body)
