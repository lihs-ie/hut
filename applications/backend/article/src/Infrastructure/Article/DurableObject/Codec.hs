{-# LANGUAGE PackageImports #-}

module Infrastructure.Article.DurableObject.Codec (articleCodec) where

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict', encode, object, withObject, (.:), (.:?), (.=))
import Data.ByteString.Lazy qualified as Lazy
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import "article" Domain.Article (
    Article (..),
    ArticleIdentifier,
    ImageReference,
    articleIdentifierText,
    imageReferenceText,
    newArticleIdentifier,
    newImageReference,
 )
import "article" Domain.Article.Common (
    DraftContent,
    DraftInput (..),
    ProofreadedContent,
    newDraftContent,
    newPublicationContent,
    proofreadContent,
    confirmAvailableImageReferences,
    draftBodyText,
    contentText,
    titleText,
 )
import "article" Domain.Article.Draft (
    draftContent,
    draftIdentifier,
    draftTimeline,
    newProofreadedDraft,
    newReadyToPublish,
    newUnvalidatedDraftWithTimeline,
    proofreadedContent,
    publicationContent,
 )
import "article" Domain.Article.Private (newPrivateArticle)
import "article" Domain.Article.Published (newPublishedArticle)
import Infrastructure.Article.DurableObject.Repository (ArticleCodec (..))
import Shared.Domain.Date (newTimeline)
import Shared.Domain.Date (Timeline)
import Shared.Domain.Error (DomainError, createUnexpectedError)
import Shared.Domain.Excerpt (excerptText, newExcerpt)
import Shared.Domain.Slug (slugText)
import Shared.Domain.Tag (TagIdentifier, tagIdentifierText)

data StoredArticle = StoredArticle
    { phase :: Text
    , identifier :: Text
    , title :: Text
    , body :: Text
    , slug :: Maybe Text
    , excerpt :: Maybe Text
    , tags :: [Text]
    , images :: [Text]
    , createdAt :: UTCTime
    , updatedAt :: UTCTime
    , publishedAt :: Maybe UTCTime
    }

instance ToJSON StoredArticle where
    toJSON value =
        object
            [ "phase" .= value.phase
            , "identifier" .= value.identifier
            , "title" .= value.title
            , "body" .= value.body
            , "slug" .= value.slug
            , "excerpt" .= value.excerpt
            , "tags" .= value.tags
            , "images" .= value.images
            , "createdAt" .= value.createdAt
            , "updatedAt" .= value.updatedAt
            , "publishedAt" .= value.publishedAt
            ]

instance FromJSON StoredArticle where
    parseJSON = withObject "StoredArticle" $ \value -> do
        phase <- value .: "phase"
        identifier <- value .: "identifier"
        title <- value .: "title"
        body <- value .: "body"
        slug <- value .:? "slug"
        excerpt <- value .:? "excerpt"
        tags <- value .: "tags"
        images <- value .: "images"
        createdAt <- value .: "createdAt"
        updatedAt <- value .: "updatedAt"
        publishedAt <- value .:? "publishedAt"
        pure
            StoredArticle
                { phase, identifier, title, body, slug, excerpt
                , tags, images, createdAt, updatedAt, publishedAt
                }

articleCodec :: ArticleCodec
articleCodec =
    ArticleCodec
        { encodeArticle = Right . decodeUtf8 . Lazy.toStrict . encode . storeArticle
        , decodeArticle = \raw -> do
            stored <- either (Left . invalidStoredArticle . Text.pack) Right (eitherDecodeStrict' (encodeUtf8 raw))
            restoreArticle stored
        }

storeArticle :: Article -> StoredArticle
storeArticle (Unvalidated draft) =
    let content = draftContent draft
     in storedBase "unvalidated" (draftIdentifier draft) (draftTimeline draft)
            (titleText content.title) (draftBodyText content.body)
            (slugText <$> content.slug) Nothing content.tags content.images Nothing
storeArticle (Proofreaded draft) =
    let content = proofreadedContent draft
     in storedBase "proofreaded" (draftIdentifier draft) (draftTimeline draft)
            (titleText content.title) (contentText content.body)
            (Just (slugText content.slug)) Nothing content.tags content.images Nothing
storeArticle (Ready draft) =
    let content = publicationContent draft
     in storedBase "ready" (draftIdentifier draft) (draftTimeline draft)
            (titleText content.title) (contentText content.body)
            (Just (slugText content.slug)) (Just (excerptText content.excerpt))
            content.tags content.images Nothing
storeArticle (Published article) =
    let content = article.publication
     in storedBase "published" article.identifier article.timeline
            (titleText content.title) (contentText content.body)
            (Just (slugText content.slug)) (Just (excerptText content.excerpt))
            content.tags content.images (Just article.publishedAt)
storeArticle (Private article) =
    let content = article.publication
     in storedBase "private" article.identifier article.timeline
            (titleText content.title) (contentText content.body)
            (Just (slugText content.slug)) (Just (excerptText content.excerpt))
            content.tags content.images (Just article.publishedAt)

storedBase ::
    Text -> ArticleIdentifier -> Timeline -> Text -> Text -> Maybe Text -> Maybe Text ->
    [TagIdentifier] -> Set.Set ImageReference -> Maybe UTCTime -> StoredArticle
storedBase phase identifier timeline title body slug excerpt tags images publishedAt =
    StoredArticle
        { phase
        , identifier = articleIdentifierText identifier
        , title
        , body
        , slug
        , excerpt
        , tags = map tagIdentifierText tags
        , images = map imageReferenceText (Set.toAscList images)
        , createdAt = timeline.createdAt
        , updatedAt = timeline.updatedAt
        , publishedAt
        }

restoreArticle :: StoredArticle -> Either DomainError Article
restoreArticle stored = do
    identifier <- newArticleIdentifier stored.identifier
    timeline <- newTimeline stored.createdAt stored.updatedAt
    references <- traverse newImageReference stored.images
    let images = Set.fromList references
    if Set.size images /= length references
        then Left (invalidStoredArticle "duplicate image references")
        else pure ()
    content <- newDraftContent (const (Right images))
        DraftInput
            { title = stored.title
            , body = stored.body
            , slug = stored.slug
            , tags = stored.tags
            }
    case stored.phase of
        "unvalidated" -> do
            requireAbsent "excerpt" stored.excerpt
            requireAbsent "publishedAt" stored.publishedAt
            pure (Unvalidated (newUnvalidatedDraftWithTimeline identifier content timeline))
        "proofreaded" -> do
            requireAbsent "excerpt" stored.excerpt
            requireAbsent "publishedAt" stored.publishedAt
            checked <- validatedContent images content
            pure (Proofreaded (newProofreadedDraft identifier checked timeline))
        "ready" -> do
            requireAbsent "publishedAt" stored.publishedAt
            checked <- validatedContent images content
            summary <- requireValue "excerpt" stored.excerpt >>= newExcerpt
            pure (Ready (newReadyToPublish identifier (newPublicationContent summary checked) timeline))
        "published" -> do
            checked <- validatedContent images content
            summary <- requireValue "excerpt" stored.excerpt >>= newExcerpt
            publishedAt <- requireValue "publishedAt" stored.publishedAt
            Published <$> newPublishedArticle identifier (newPublicationContent summary checked) timeline publishedAt
        "private" -> do
            checked <- validatedContent images content
            summary <- requireValue "excerpt" stored.excerpt >>= newExcerpt
            publishedAt <- requireValue "publishedAt" stored.publishedAt
            Private <$> newPrivateArticle identifier (newPublicationContent summary checked) timeline publishedAt
        _ -> Left (invalidStoredArticle "unknown article phase")

validatedContent :: Set.Set ImageReference -> DraftContent -> Either DomainError ProofreadedContent
validatedContent images content = do
    available <- confirmAvailableImageReferences images images
    proofreadContent available content

requireValue :: Text -> Maybe a -> Either DomainError a
requireValue field = maybe (Left (invalidStoredArticle (field <> " is missing"))) Right

requireAbsent :: Text -> Maybe a -> Either DomainError ()
requireAbsent field = maybe (Right ()) (const (Left (invalidStoredArticle (field <> " must be absent"))))

invalidStoredArticle :: Text -> DomainError
invalidStoredArticle = createUnexpectedError "ArticleStorage"
