module Media.Domain.ImageUsage (
    SourceKind (..),
    SourceIdentifier,
    SourcePosition,
    ImageReference,
    ImageUsageProjection,
    newSourceIdentifier,
    newSourcePosition,
    newImageReference,
    newImageUsageProjection,
    sourceIdentifierText,
    sourcePositionText,
    imageReferenceText,
    foldImageUsageProjection,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Shared.Domain.Error (DomainError, createInvariantViolation)

data SourceKind = ArticleSource | MemoSource | SeriesSource
    deriving stock (Show, Eq)

newtype SourceIdentifier = SourceIdentifier Text
    deriving stock (Show, Eq)

newtype SourcePosition = SourcePosition Text
    deriving stock (Show, Eq, Ord)

newtype ImageReference = ImageReference Text
    deriving stock (Show, Eq, Ord)

data ImageUsageProjection = ImageUsageProjection
    { sourceKind :: SourceKind
    , source :: SourceIdentifier
    , position :: SourcePosition
    , references :: [ImageReference]
    , referencedAt :: UTCTime
    }
    deriving stock (Show, Eq)

newSourceIdentifier :: Text -> Either DomainError SourceIdentifier
newSourceIdentifier = nonBlank SourceIdentifier "source identifier"

newSourcePosition :: Text -> Either DomainError SourcePosition
newSourcePosition = nonBlank SourcePosition "source position"

newImageReference :: Text -> Either DomainError ImageReference
newImageReference = nonBlank ImageReference "image reference"

newImageUsageProjection ::
    SourceKind ->
    SourceIdentifier ->
    SourcePosition ->
    [ImageReference] ->
    UTCTime ->
    ImageUsageProjection
newImageUsageProjection kind sourceIdentifier sourcePosition imageReferences =
    ImageUsageProjection kind sourceIdentifier sourcePosition (deduplicate imageReferences)

sourceIdentifierText :: SourceIdentifier -> Text
sourceIdentifierText (SourceIdentifier value) = value

sourcePositionText :: SourcePosition -> Text
sourcePositionText (SourcePosition value) = value

imageReferenceText :: ImageReference -> Text
imageReferenceText (ImageReference value) = value

foldImageUsageProjection ::
    (SourceKind -> SourceIdentifier -> SourcePosition -> [ImageReference] -> UTCTime -> result) ->
    ImageUsageProjection ->
    result
foldImageUsageProjection
    transform
    (ImageUsageProjection kind sourceIdentifier sourcePosition imageReferences referencedAt) =
        transform kind sourceIdentifier sourcePosition imageReferences referencedAt

nonBlank :: (Text -> value) -> Text -> Text -> Either DomainError value
nonBlank constructor name value
    | Text.null (Text.strip value) =
        Left (createInvariantViolation name "value must not be blank")
    | otherwise = Right (constructor value)

deduplicate :: (Eq value) => [value] -> [value]
deduplicate = foldr (\value values -> if value `elem` values then values else value : values) []
