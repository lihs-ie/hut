module UseCase.Transaction.ReadSpec (run) where

import Control.Monad (forM_)
import Data.IORef
import Domain.Article (Article (..))
import Domain.Article.Draft qualified as Draft
import Domain.Article.Published qualified as Published
import Shared.Domain.Excerpt (newExcerpt)
import Shared.Domain.Slug (slugText)
import TestSupport
import UseCase.BrowseArticlesForAdmin qualified as Admin
import UseCase.BrowseArticlesForReader qualified as Reader
import UseCase.CheckSlugAvailability qualified as Slug
import UseCase.ReadArticle qualified as Read
import UseCase.Reading (ArticleFilter (..))
import UseCase.TestSupport (command)
import UseCase.Transaction.Fixture
import UseCase.ViewArticleForAdmin qualified as View

run :: IO ()
run = do
    value <- right identifier
    initial <- right start
    available <- right confirmed
    proof <- right (Draft.proofread (timestamp 1) available initial)
    excerpt <- right (newExcerpt "Summary")
    ready <- right (Draft.prepareToPublish (timestamp 2) excerpt proof)
    published <- right (Published.publish (timestamp 3) ready)
    admin <- command (Admin.BrowseArticlesForAdminPayload 1 Nothing AllArticles)
    reader <- command (Reader.BrowseArticlesForReaderPayload 1 Nothing Nothing [])
    view <- command (View.ViewArticleForAdminPayload value)
    readCommand <- command (Read.ReadArticlePayload (slugText published.publication.slug))
    slug <- command (Slug.CheckSlugAvailabilityPayload value (slugText published.publication.slug))
    let cases =
            [ (["browse-admin"], \f -> (() <$) <$> Admin.browseArticlesForAdmin (Admin.Dependencies f.manager searchArticles) admin)
            , (["browse-reader"], \f -> (() <$) <$> Reader.browseArticlesForReader (Reader.Dependencies f.manager searchPublished) reader)
            , (["find"], \f -> (() <$) <$> View.viewArticleForAdmin (View.Dependencies f.manager findArticle) view)
            , (["slug"], \f -> (() <$) <$> Read.readArticle (Read.Dependencies f.manager findBySlug) readCommand)
            , (["find", "owner"], \f -> (() <$) <$> Slug.checkSlugAvailability (Slug.Dependencies f.manager findArticle findSlugOwner) slug)
            ]
    forM_ cases $ \(operations, invoke) -> do
        fixture <- newFixture (Just (Published published)) NoFailure
        result <- invoke fixture
        check "read success" (result == Right ())
        check "one consistent snapshot boundary" . (== ["begin"] <> operations <> ["commit"]) =<< readIORef fixture.trace
        check "read has no writes" . (== Store (Just (Published published)) []) =<< readIORef fixture.stored
