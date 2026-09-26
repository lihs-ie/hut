{-# LANGUAGE PackageImports #-}

module Presentation.Handler.API.Reading (
    ReadingHandlerDependencies (..),
    browseAdminHandler,
    viewAdminHandler,
    checkSlugHandler,
    browseReaderHandler,
    readArticleHandler,
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Presentation.API (CorrelatedResponse, SlugAvailabilityResponse (..))
import Presentation.API.ArticleView (ArticlePage (..), ArticleView, articleView, pageView, publishedView)
import Presentation.Handler.API.Error (domainErrorResponse, publicError)
import Presentation.Handler.API.Metadata (MetadataDependencies, newCommand, parseArticleIdentifier)
import Servant.API (addHeader)
import Servant.Cloudflare.Workers.Handler (Handler)
import "article" Domain.Article.Criteria (ArticleFilter (..))
import "article" UseCase.BrowseArticlesForAdmin qualified as BrowseAdmin
import "article" UseCase.BrowseArticlesForReader qualified as BrowseReader
import "article" UseCase.CheckSlugAvailability qualified as CheckSlug
import "article" UseCase.ReadArticle qualified as ReadArticle
import "article" UseCase.ViewArticleForAdmin qualified as ViewAdmin
import "shared" Shared.Domain.Error (DomainError)
import "shared" Shared.UseCase.Command (Command (..))
import Text.Read (readMaybe)

data ReadingHandlerDependencies = ReadingHandlerDependencies
    { metadata :: MetadataDependencies
    , browseAdmin :: BrowseAdmin.BrowseArticlesForAdminCommand ->
        IO (Either DomainError BrowseAdmin.BrowseArticlesForAdminResult)
    , viewAdmin :: ViewAdmin.ViewArticleForAdminCommand ->
        IO (Either DomainError ViewAdmin.ViewArticleForAdminResult)
    , checkSlug :: CheckSlug.CheckSlugAvailabilityCommand ->
        IO (Either DomainError CheckSlug.CheckSlugAvailabilityResult)
    , browseReader :: BrowseReader.BrowseArticlesForReaderCommand ->
        IO (Either DomainError BrowseReader.BrowseArticlesForReaderResult)
    , readArticle :: ReadArticle.ReadArticleCommand ->
        IO (Either DomainError ReadArticle.ReadArticleResult)
    }

browseAdminHandler ::
    ReadingHandlerDependencies -> Maybe Text -> Maybe Text ->
    Maybe Text -> Maybe Text -> Maybe Text ->
    Handler env (CorrelatedResponse ArticlePage)
browseAdminHandler dependencies actorHeader correlationHeader rawPage rawSize rawStatus = do
    (metadata, correlation) <- newCommand dependencies.metadata actorHeader correlationHeader ()
    page <- maybe (pure 1) (parsePositive "page" correlation) rawPage
    size <- traverse (parsePositive "size" correlation) rawSize
    status <- parseStatus correlation rawStatus
    let command = metadata{payload = BrowseAdmin.BrowseArticlesForAdminPayload page size status}
    result <- liftIO (dependencies.browseAdmin command)
    found <- either (throwError . domainErrorResponse correlation) pure result
    pure $ addHeader correlation $ ArticlePage
        (map articleView found.articles) (pageView found.pager)

viewAdminHandler ::
    ReadingHandlerDependencies -> Maybe Text -> Maybe Text -> Text ->
    Handler env (CorrelatedResponse ArticleView)
viewAdminHandler dependencies actorHeader correlationHeader rawArticle = do
    (metadata, correlation) <- newCommand dependencies.metadata actorHeader correlationHeader ()
    identifier <- parseArticleIdentifier correlation rawArticle
    let command = metadata{payload = ViewAdmin.ViewArticleForAdminPayload identifier}
    result <- liftIO (dependencies.viewAdmin command)
    found <- either (throwError . domainErrorResponse correlation) pure result
    pure (addHeader correlation (articleView found.article))

checkSlugHandler ::
    ReadingHandlerDependencies -> Maybe Text -> Maybe Text -> Text -> Maybe Text ->
    Handler env (CorrelatedResponse SlugAvailabilityResponse)
checkSlugHandler dependencies actorHeader correlationHeader rawArticle rawSlug = do
    (metadata, correlation) <- newCommand dependencies.metadata actorHeader correlationHeader ()
    identifier <- parseArticleIdentifier correlation rawArticle
    slug <- maybe (throwError (publicError 400 "missing_slug" correlation)) pure rawSlug
    let command = metadata{payload = CheckSlug.CheckSlugAvailabilityPayload identifier slug}
    result <- liftIO (dependencies.checkSlug command)
    found <- either (throwError . domainErrorResponse correlation) pure result
    pure $ addHeader correlation $ SlugAvailabilityResponse
        (found.availability == CheckSlug.Available)

browseReaderHandler ::
    ReadingHandlerDependencies -> Maybe Text -> Maybe Text -> Maybe Text ->
    Handler env (CorrelatedResponse ArticlePage)
browseReaderHandler dependencies correlationHeader rawPage rawSize = do
    (metadata, correlation) <- newCommand dependencies.metadata
        (Just "reader") correlationHeader ()
    page <- maybe (pure 1) (parsePositive "page" correlation) rawPage
    size <- traverse (parsePositive "size" correlation) rawSize
    let command = metadata{payload = BrowseReader.BrowseArticlesForReaderPayload page size}
    result <- liftIO (dependencies.browseReader command)
    found <- either (throwError . domainErrorResponse correlation) pure result
    pure $ addHeader correlation $ ArticlePage
        (map publishedView found.articles) (pageView found.pager)

readArticleHandler ::
    ReadingHandlerDependencies -> Maybe Text -> Text ->
    Handler env (CorrelatedResponse ArticleView)
readArticleHandler dependencies correlationHeader slug = do
    (metadata, correlation) <- newCommand dependencies.metadata
        (Just "reader") correlationHeader ()
    let command = metadata{payload = ReadArticle.ReadArticlePayload slug}
    result <- liftIO (dependencies.readArticle command)
    found <- either (throwError . domainErrorResponse correlation) pure result
    pure (addHeader correlation (publishedView found.article))

parsePositive :: Text -> Text -> Text -> Handler env Int
parsePositive field correlation raw =
    case readMaybe (Text.unpack raw) :: Maybe Integer of
        Just value
            | Text.length raw <= length (show (maxBound :: Int))
                && Text.all isAsciiDigit raw
                && value > 0
                && value <= toInteger (maxBound :: Int) ->
                pure (fromInteger value)
        _ -> throwError (publicError 400 ("invalid_" <> field) correlation)
  where
    isAsciiDigit character = character >= '0' && character <= '9'

parseStatus :: Text -> Maybe Text -> Handler env ArticleFilter
parseStatus _ Nothing = pure AllArticles
parseStatus _ (Just "all") = pure AllArticles
parseStatus _ (Just "unvalidated") = pure UnvalidatedOnly
parseStatus _ (Just "proofreaded") = pure ProofreadedOnly
parseStatus _ (Just "ready") = pure ReadyOnly
parseStatus _ (Just "published") = pure PublishedOnly
parseStatus _ (Just "private") = pure PrivateOnly
parseStatus correlation _ = throwError (publicError 400 "invalid_status" correlation)
