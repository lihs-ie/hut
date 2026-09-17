module Media.Infrastructure.D1.ImageUsageRepository (
    newImageUsageProjectionStore,
) where

import Cloudflare.Workers.Binding.D1 (D1, D1Value (..))
import Cloudflare.Workers.Binding.D1.Query (
    D1Statement (..),
    D1ValueDecoder,
    d1Column,
    d1ExecuteBatch,
    d1QueryFirst,
    d1Refine,
    d1Text,
 )
import Control.Exception (SomeException, fromException, throwIO, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Shared.Domain.Error (DomainError, createServiceUnavailable)
import "media" Media.Domain.ImageUsage
import "media" Media.UseCase.ProjectImageUsage

newImageUsageProjectionStore :: D1 -> ImageUsageProjectionStore
newImageUsageProjectionStore database = ImageUsageProjectionStore (replaceProjection database)

data ProjectionCheckpoint = ProjectionCheckpoint
    { sourcePosition :: Text
    , eventIdentifier :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

replaceProjection :: D1 -> Text -> ImageUsageProjection -> IO ProjectionApplyResult
replaceProjection database eventIdentifier projection =
    databaseOperation
        "replace image usage projection"
        (foldImageUsageProjection apply projection)
  where
    apply sourceKind source position references referencedAt = do
        let kind = sourceKindText sourceKind
            sourceText = sourceIdentifierText source
            stream = kind <> ":" <> sourceText
            positionText = sourcePositionText position
        current <-
            d1QueryFirst
                database
                (D1Statement findProjectionPositionSQL [D1Text stream])
                (d1Column "position" checkpointDecoder)
        case comparePosition current eventIdentifier positionText of
            ProjectionDuplicate -> pure ProjectionDuplicate
            ProjectionOutOfOrder -> pure ProjectionOutOfOrder
            ProjectionApplied -> do
                let replacePosition =
                        D1Statement
                            replaceProjectionPositionSQL
                            [ D1Text stream
                            , D1Text (checkpointText positionText eventIdentifier)
                            , D1Text (timeText referencedAt)
                            ]
                    deleteExisting =
                        D1Statement
                            "DELETE FROM image_usages WHERE source_kind=? AND source_identifier=?"
                            [D1Text kind, D1Text sourceText]
                    inserts = fmap (insertUsage kind sourceText referencedAt) references
                _ <- d1ExecuteBatch database (replacePosition : deleteExisting : inserts)
                pure ProjectionApplied

insertUsage :: Text -> Text -> UTCTime -> ImageReference -> D1Statement
insertUsage kind source referencedAt reference =
    D1Statement
        insertImageUsageSQL
        [ D1Text (imageReferenceText reference)
        , D1Text kind
        , D1Text source
        , D1Text (timeText referencedAt)
        ]

comparePosition :: Maybe ProjectionCheckpoint -> Text -> Text -> ProjectionApplyResult
comparePosition Nothing _ _ = ProjectionApplied
comparePosition (Just current) incomingEvent incomingPosition
    | incomingEvent == current.eventIdentifier = ProjectionDuplicate
    | decimalTextCompare incomingPosition current.sourcePosition == GT = ProjectionApplied
    | otherwise = ProjectionOutOfOrder

checkpointDecoder :: D1ValueDecoder ProjectionCheckpoint
checkpointDecoder = d1Refine decodeCheckpoint d1Text

decodeCheckpoint :: Text -> Either Text ProjectionCheckpoint
decodeCheckpoint value =
    either
        (Left . Text.pack)
        Right
        (eitherDecodeStrict' (TextEncoding.encodeUtf8 value))

checkpointText :: Text -> Text -> Text
checkpointText position event =
    TextEncoding.decodeUtf8
        . LazyByteString.toStrict
        $ encode (ProjectionCheckpoint position event)

findProjectionPositionSQL :: Text
findProjectionPositionSQL =
    Text.unwords
        [ "SELECT position FROM projection_positions"
        , "WHERE projection_name='media-image-usage' AND stream_name=?"
        ]

replaceProjectionPositionSQL :: Text
replaceProjectionPositionSQL =
    Text.unwords
        [ "INSERT INTO projection_positions"
        , "(projection_name,stream_name,position,applied_at)"
        , "VALUES ('media-image-usage',?,?,?)"
        , "ON CONFLICT(projection_name,stream_name)"
        , "DO UPDATE SET position=excluded.position,applied_at=excluded.applied_at"
        ]

insertImageUsageSQL :: Text
insertImageUsageSQL =
    Text.unwords
        [ "INSERT INTO image_usages"
        , "(image_identifier,source_kind,source_identifier,referenced_at)"
        , "VALUES (?,?,?,?)"
        ]

decimalTextCompare :: Text -> Text -> Ordering
decimalTextCompare left right = compare (Text.length left, left) (Text.length right, right)

databaseOperation :: Text -> IO value -> IO value
databaseOperation operation action = do
    outcome <- try @SomeException action
    either (throwIO . toDomainError) pure outcome
  where
    toDomainError exception =
        maybe
            ( createServiceUnavailable
                "MediaDatabase"
                (operation <> " failed")
            )
            id
            (fromException exception :: Maybe DomainError)

sourceKindText :: SourceKind -> Text
sourceKindText ArticleSource = "article"
sourceKindText MemoSource = "memo"
sourceKindText SeriesSource = "series"

timeText :: UTCTime -> Text
timeText = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
