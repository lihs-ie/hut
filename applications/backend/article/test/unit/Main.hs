module Main (main) where

import Article.Worker.API.CompositionSpec qualified as APIComposition
import Article.Worker.Excerpt.ContractSpec qualified as ExcerptContract
import Article.Worker.Excerpt.DeadLetterSpec qualified as ExcerptDeadLetter
import Article.Worker.Completion.ContractSpec qualified as CompletionContract
import Article.Worker.DO.AlarmSpec qualified as DOAlarm
import Domain.Article.EventSpec qualified as Event
import Domain.Article.CommonSpec qualified as Common
import Domain.Article.CriteriaSpec qualified as Criteria
import Domain.Article.DraftSpec qualified as Draft
import Domain.Article.LifecycleSpec qualified as Lifecycle
import Infrastructure.Article.Excerpt.WorkersAISpec qualified as WorkersAI
import Infrastructure.Article.DurableObject.TransactionSpec qualified as DOTransaction
import Infrastructure.Article.DurableObject.AlarmScheduleSpec qualified as DOAlarmSchedule
import Infrastructure.Article.DurableObject.RepositorySpec qualified as DORepository
import Infrastructure.Article.DurableObject.QuerySpec qualified as DOQuery
import Infrastructure.Article.DurableObject.CodecSpec qualified as DOCodec
import Infrastructure.Article.DurableObject.OutboxSpec qualified as DOOutbox
import Infrastructure.Article.DurableObject.MediaOutboxSpec qualified as DOMediaOutbox
import Infrastructure.Article.DurableObject.LogOutboxSpec qualified as DOLogOutbox
import Infrastructure.Article.DurableObject.GenerationJobSpec qualified as DOGenerationJob
import Infrastructure.Article.DurableObject.CompletionSpec qualified as DOCompletion
import Infrastructure.Article.DurableObject.ReadyOutboxSpec qualified as DOReadyOutbox
import Infrastructure.Article.DurableObject.ProofreadOutboxSpec qualified as DOProofreadOutbox
import Infrastructure.Article.Queue.ExcerptGenerationSpec qualified as ExcerptGeneration
import Infrastructure.Article.Media.ImageAvailabilitySpec qualified as ImageAvailability
import Infrastructure.Article.Media.ImageReferencesSpec qualified as ImageReferences
import Presentation.Handler.DO.ExcerptClaimSpec qualified as ExcerptClaim
import Presentation.Handler.DO.ExcerptCompleteSpec qualified as ExcerptComplete
import Presentation.Handler.DO.ExcerptAbandonSpec qualified as ExcerptAbandon
import Presentation.Handler.Queue.ExcerptGenerationSpec qualified as GenerationHandler
import Presentation.Handler.Queue.ExcerptCompletionSpec qualified as CompletionHandler
import Presentation.Server.APISpec qualified as APIServer
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
import UseCase.RequestExcerptRegenerationSpec qualified as RequestExcerptRegeneration
import UseCase.ReadingSpec qualified as Reading
import UseCase.ResumePublicationSpec qualified as ResumePublication
import UseCase.TakeDownSpec qualified as TakeDown
import UseCase.Transaction.ProofreadSpec qualified as TransactionProofread
import UseCase.Transaction.ReadSpec qualified as TransactionRead
import UseCase.Transaction.VersionSpec qualified as TransactionVersion
import UseCase.Transaction.WriteSpec qualified as TransactionWrite
import UseCase.ViewArticleForAdminSpec qualified as ViewAdmin

main :: IO ()
main = do
    APIComposition.run
    ExcerptContract.run
    ExcerptDeadLetter.run
    CompletionContract.run
    DOAlarm.run
    Common.run
    Event.run
    Draft.run
    Lifecycle.run
    WorkersAI.run
    _ <- DOTransaction.run
    DOAlarmSchedule.run
    DORepository.run
    DOQuery.run
    DOCodec.run
    DOOutbox.run
    DOMediaOutbox.run
    DOLogOutbox.run
    DOGenerationJob.run
    DOCompletion.run
    DOReadyOutbox.run
    DOProofreadOutbox.run
    ExcerptGeneration.run
    ImageAvailability.run
    ImageReferences.run
    ExcerptClaim.run
    ExcerptComplete.run
    ExcerptAbandon.run
    GenerationHandler.run
    CompletionHandler.run
    APIServer.run
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
    RequestExcerptRegeneration.run
    CheckSlug.run
    ResumePublication.run
    DiscardArticle.run
    TransactionWrite.run
    TransactionVersion.run
    Criteria.run
    TransactionRead.run
    TransactionProofread.run
    putStrLn "Article domain and use-case tests passed"
