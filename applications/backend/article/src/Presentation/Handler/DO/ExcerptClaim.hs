{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Presentation.Handler.DO.ExcerptClaim (
    handleExcerptClaim,
) where

import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (
    Method (GET),
    Request,
    Response (..),
    ResponseBody (ResponseBodyBytes),
    Status (Status),
    requestMethod,
    requestPath,
    requestURL,
 )
import Cloudflare.Workers.URL (urlQueryParamOccurrences)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (newIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import "article" Domain.Article (
    Article (Proofreaded),
    articleIdentifierText,
    imageReferenceText,
    newArticleIdentifier,
 )
import "article" Domain.Article.Common (ProofreadedContent, contentText, titleText)
import "article" Domain.Article.Draft (proofreadedContent)
import Infrastructure.Article.DurableObject.Codec (articleCodec)
import Infrastructure.Article.DurableObject.GenerationJob (
    GenerationClaim (..),
    claimGenerationWith,
 )
import Infrastructure.Article.DurableObject.Repository (ExecuteSQL, findArticleWith)
import Infrastructure.Article.Queue.ExcerptGeneration (
    ExcerptGenerationRequested (..),
    generationRequestIdentifierText,
    newGenerationRequestIdentifier,
 )
import "shared" Shared.Domain.Common.Primitive (newPositiveInteger)
import "shared" Shared.Domain.Slug (slugText)
import "shared" Shared.Domain.Tag (tagIdentifierText)
import "shared" Shared.Infrastructure.Versioning (
    Version,
    emptyVersionContext,
    newVersion,
    versionInteger,
 )
import Text.Read (readMaybe)

-- Supply doStorageTransactionWith for the DO's storage. Both SQL reads must
-- use that same storage, so the claim and aggregate form one snapshot.
handleExcerptClaim ::
    (forall value. IO value -> IO value) ->
    ExecuteSQL ->
    Request ->
    IO Response
handleExcerptClaim withinTransaction execute request =
    case parseClaimRequest request of
        Nothing -> pure badRequest
        Just generation -> do
            result <- try @SomeException $ withinTransaction $ do
                versions <- newIORef $ emptyVersionContext "Article" articleIdentifierText
                claim <- claimGenerationWith execute generation
                case claim of
                    Left err -> pure (Left err)
                    Right GenerationTerminalAck -> pure (Right Nothing)
                    Right GenerationClaimed -> do
                        found <- findArticleWith execute versions articleCodec generation.article
                        pure $ case found of
                            Left err -> Left err
                            Right (Just (Proofreaded draft)) ->
                                Right (Just (proofreadedContent draft))
                            Right _ -> Right Nothing
            case result of
                Left exception
                    | Just asynchronous <- fromException @SomeAsyncException exception ->
                        throwIO asynchronous
                    | otherwise -> pure storageFailure
                Right (Left _) -> pure storageFailure
                Right (Right Nothing) -> pure obsolete
                Right (Right (Just content)) -> pure $ claimed generation content

parseClaimRequest :: Request -> Maybe ExcerptGenerationRequested
parseClaimRequest request
    | requestMethod request /= GET = Nothing
    | requestPath request /= "/internal/excerpt-generation/claim" = Nothing
    | otherwise = do
        rawArticle <- single "article"
        rawRequest <- single "request"
        rawRevision <- single "expectedRevision"
        article <- either (const Nothing) Just (newArticleIdentifier rawArticle)
        identifier <- either (const Nothing) Just (newGenerationRequestIdentifier rawRequest)
        revision <- parseRevision rawRevision
        pure (ExcerptGenerationRequested identifier article revision)
  where
    single name = case urlQueryParamOccurrences name (requestURL request) of
        [Just value] | not (Text.null value) -> Just value
        _ -> Nothing

parseRevision :: Text -> Maybe Version
parseRevision raw
    | Text.null raw = Nothing
    | Text.length raw > 16 = Nothing
    | not (Text.all isAsciiDigit raw) = Nothing
    | otherwise = do
        value <- readMaybe (Text.unpack raw)
        if value > 9007199254740991
            then Nothing
            else newVersion <$> either (const Nothing) Just (newPositiveInteger value)
  where
    isAsciiDigit character = character >= '0' && character <= '9'

claimed :: ExcerptGenerationRequested -> ProofreadedContent -> Response
claimed generation content =
    Response
        { responseStatus = Status 200
        , responseHeaders = headersFromList
            [ ("content-type", "application/json; charset=utf-8")
            , ("cache-control", "no-store")
            ]
        , responseBody = ResponseBodyBytes $ Lazy.toStrict $ encode $ object
            [ "article" .= articleIdentifierText generation.article
            , "request" .= generationRequestIdentifierText generation.identifier
            , "expectedRevision" .= versionInteger generation.expectedRevision
            , "content" .= object
                [ "title" .= titleText content.title
                , "body" .= contentText content.body
                , "slug" .= slugText content.slug
                , "tags" .= map tagIdentifierText content.tags
                , "images" .= map imageReferenceText (Set.toAscList content.images)
                ]
            ]
        }

badRequest :: Response
badRequest = emptyResponse 400

obsolete :: Response
obsolete = emptyResponse 204

storageFailure :: Response
storageFailure = emptyResponse 500

emptyResponse :: Int -> Response
emptyResponse status =
    Response
        { responseStatus = Status status
        , responseHeaders = headersFromList [("cache-control", "no-store")]
        , responseBody = ResponseBodyBytes ""
        }
