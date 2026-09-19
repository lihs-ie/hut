module Main (main) where

import Domain.Article.CommonSpec qualified as Common
import Domain.Article.DraftSpec qualified as Draft
import Domain.Article.LifecycleSpec qualified as Lifecycle
import UseCase.AmendDraftSpec qualified as AmendDraft
import UseCase.BrowseArticlesForAdminSpec qualified as BrowseAdmin
import UseCase.BrowseArticlesForReaderSpec qualified as BrowseReader
import UseCase.CheckSlugAvailabilitySpec qualified as CheckSlug
import UseCase.DiscardArticleSpec qualified as DiscardArticle
import UseCase.GenerationRevisionSpec qualified as GenerationRevision
import UseCase.JotDownSpec qualified as JotDown
import UseCase.PrepareToPublishSpec qualified as PrepareToPublish
import UseCase.ProofreadSpec qualified as Proofread
import UseCase.PublishSpec qualified as Publish
import UseCase.ReadArticleSpec qualified as ReadArticle
import UseCase.ReadingSpec qualified as Reading
import UseCase.ResumePublicationSpec qualified as ResumePublication
import UseCase.TakeDownSpec qualified as TakeDown
import UseCase.ViewArticleForAdminSpec qualified as ViewAdmin

main :: IO ()
main = do
    Common.run
    Draft.run
    Lifecycle.run
    JotDown.run
    AmendDraft.run
    Proofread.run
    PrepareToPublish.run
    GenerationRevision.run
    Publish.run
    TakeDown.run
    Reading.run
    BrowseAdmin.run
    ViewAdmin.run
    BrowseReader.run
    ReadArticle.run
    CheckSlug.run
    ResumePublication.run
    DiscardArticle.run
    putStrLn "Article domain and use-case tests passed"
