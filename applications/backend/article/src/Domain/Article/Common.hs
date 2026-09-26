module Domain.Article.Common (
    ArticleIdentifier,
    newArticleIdentifier,
    articleIdentifierText,
    ImageReference,
    newImageReference,
    imageReferenceText,
    Title,
    newTitle,
    titleText,
    DraftBody,
    newDraftBody,
    draftBodyText,
    Content,
    newContent,
    contentText,
    DraftInput (..),
    ExtractImageReferences,
    DraftContent,
    newDraftContent,
    AvailableImageReferences,
    confirmAvailableImageReferences,
    ProofreadedContent,
    proofreadContent,
    PublicationContent,
    newPublicationContent,
    replaceExcerpt,
    amendTimeline,
) where

import Data.List (nub)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import GHC.Records (HasField (..))
import Shared.Domain.Date (Timeline, newTimeline)
import Shared.Domain.Error (DomainError, createInvariantViolation)
import Shared.Domain.Excerpt (Excerpt)
import Shared.Domain.Identifier (ULID, newULID, ulidText)
import Shared.Domain.Slug (Slug, newSlug)
import Shared.Domain.Tag (TagIdentifier, newTagIdentifier)

newtype ArticleIdentifier = ArticleIdentifier ULID
    deriving stock (Show, Eq, Ord)

newArticleIdentifier :: Text -> Either DomainError ArticleIdentifier
newArticleIdentifier = fmap ArticleIdentifier . newULID

articleIdentifierText :: ArticleIdentifier -> Text
articleIdentifierText (ArticleIdentifier value) = ulidText value

-- Article owns this reference type; Media owns the image aggregate.
newtype ImageReference = ImageReference ULID
    deriving stock (Show, Eq, Ord)

newImageReference :: Text -> Either DomainError ImageReference
newImageReference = fmap ImageReference . newULID

imageReferenceText :: ImageReference -> Text
imageReferenceText (ImageReference value) = ulidText value

newtype Title = Title Text
    deriving stock (Show, Eq)

newTitle :: Text -> Either DomainError Title
newTitle value
    | Text.null (Text.strip value) || Text.length value > 100 =
        Left (createInvariantViolation "Title" "length must be between 1 and 100")
    | otherwise = Right (Title value)

titleText :: Title -> Text
titleText (Title value) = value

newtype DraftBody = DraftBody Text
    deriving stock (Show, Eq)

newDraftBody :: Text -> DraftBody
newDraftBody = DraftBody

draftBodyText :: DraftBody -> Text
draftBodyText (DraftBody value) = value

newtype Content = Content Text
    deriving stock (Show, Eq)

newContent :: Text -> Either DomainError Content
newContent value
    | Text.null (Text.strip value) =
        Left (createInvariantViolation "Content" "content must not be blank")
    | otherwise = Right (Content value)

contentText :: Content -> Text
contentText (Content value) = value

data DraftInput = DraftInput
    { title :: Text
    , body :: Text
    , slug :: Maybe Text
    , tags :: [Text]
    }
    deriving stock (Show, Eq)

type ExtractImageReferences = DraftBody -> Either DomainError (Set ImageReference)

data DraftContent = DraftContent
    { storedTitle :: Title
    , storedBody :: DraftBody
    , storedSlug :: Maybe Slug
    , storedTags :: [TagIdentifier]
    , storedImages :: Set ImageReference
    }
    deriving stock (Show, Eq)

newDraftContent ::
    ExtractImageReferences -> DraftInput -> Either DomainError DraftContent
newDraftContent extract input = do
    title <- newTitle input.title
    slug <- traverse newSlug input.slug
    tags <- nub <$> traverse newTagIdentifier input.tags
    let body = newDraftBody input.body
    images <- extract body
    pure DraftContent{storedTitle = title, storedBody = body, storedSlug = slug, storedTags = tags, storedImages = images}

newtype AvailableImageReferences = AvailableImageReferences (Set ImageReference)
    deriving stock (Show, Eq)

-- The use case supplies Media's successful availability check, never client input.
confirmAvailableImageReferences ::
    Set ImageReference ->
    Set ImageReference ->
    Either DomainError AvailableImageReferences
confirmAvailableImageReferences requested available
    | requested == available = Right (AvailableImageReferences requested)
    | otherwise =
        Left
            (createInvariantViolation "Images" "all referenced images must be available")

data ProofreadedContent = ProofreadedContent
    { storedTitle :: Title
    , storedBody :: Content
    , storedSlug :: Slug
    , storedTags :: [TagIdentifier]
    , storedImages :: Set ImageReference
    }
    deriving stock (Show, Eq)

proofreadContent ::
    AvailableImageReferences -> DraftContent -> Either DomainError ProofreadedContent
proofreadContent (AvailableImageReferences available) draft = do
    body <- newContent (draftBodyText draft.body)
    slug <-
        maybe
            (Left (createInvariantViolation "Slug" "slug is required to proofread"))
            Right
            draft.slug
    if available /= draft.images
        then Left (createInvariantViolation "Images" "confirmation does not match this draft")
        else
            pure
                ProofreadedContent
                    { storedTitle = draft.title
                    , storedBody = body
                    , storedSlug = slug
                    , storedTags = draft.tags
                    , storedImages = draft.images
                    }

data PublicationContent = PublicationContent
    { storedTitle :: Title
    , storedBody :: Content
    , storedSlug :: Slug
    , storedExcerpt :: Excerpt
    , storedTags :: [TagIdentifier]
    , storedImages :: Set ImageReference
    }
    deriving stock (Show, Eq)

newPublicationContent :: Excerpt -> ProofreadedContent -> PublicationContent
newPublicationContent excerpt content =
    PublicationContent
        { storedTitle = content.title
        , storedBody = content.body
        , storedSlug = content.slug
        , storedExcerpt = excerpt
        , storedTags = content.tags
        , storedImages = content.images
        }

replaceExcerpt :: Excerpt -> PublicationContent -> PublicationContent
replaceExcerpt excerpt content = content{storedExcerpt = excerpt}

amendTimeline :: UTCTime -> Timeline -> Either DomainError Timeline
amendTimeline timestamp previous
    | timestamp < previous.updatedAt =
        Left (createInvariantViolation "Timeline" "timestamp must not precede updatedAt")
    | otherwise = newTimeline previous.createdAt timestamp

instance HasField "title" DraftContent (Title) where
    getField value = value.storedTitle

instance HasField "body" DraftContent (DraftBody) where
    getField value = value.storedBody

instance HasField "slug" DraftContent (Maybe Slug) where
    getField value = value.storedSlug

instance HasField "tags" DraftContent ([TagIdentifier]) where
    getField value = value.storedTags

instance HasField "images" DraftContent (Set ImageReference) where
    getField value = value.storedImages

instance HasField "title" ProofreadedContent (Title) where
    getField value = value.storedTitle

instance HasField "body" ProofreadedContent (Content) where
    getField value = value.storedBody

instance HasField "slug" ProofreadedContent (Slug) where
    getField value = value.storedSlug

instance HasField "tags" ProofreadedContent ([TagIdentifier]) where
    getField value = value.storedTags

instance HasField "images" ProofreadedContent (Set ImageReference) where
    getField value = value.storedImages

instance HasField "title" PublicationContent (Title) where
    getField value = value.storedTitle

instance HasField "body" PublicationContent (Content) where
    getField value = value.storedBody

instance HasField "slug" PublicationContent (Slug) where
    getField value = value.storedSlug

instance HasField "excerpt" PublicationContent (Excerpt) where
    getField value = value.storedExcerpt

instance HasField "tags" PublicationContent ([TagIdentifier]) where
    getField value = value.storedTags

instance HasField "images" PublicationContent (Set ImageReference) where
    getField value = value.storedImages
