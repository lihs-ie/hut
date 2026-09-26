module Media.Infrastructure.D1.ImageRepository (
    newInspectionDependencies,
    newRetentionDependencies,
    newImageIdentifierResult,
    newUploadAttemptIdentifierResult,
    persistImageResult,
    findImageResult,
    findAvailableImagesResult,
) where

import Cloudflare.Workers.Binding.D1 (
    D1,
    D1Meta (d1MetaChanges),
    D1RunResult (d1RunResultMeta),
    D1Value (..),
 )
import Cloudflare.Workers.Binding.D1.Query (
    D1RowDecoder,
    D1Statement (..),
    D1ValueDecoder,
    d1Column,
    d1Execute,
    d1ExecuteBatch,
    d1Integer,
    d1Nullable,
    d1Query,
    d1QueryFirst,
    d1Refine,
    d1Text,
 )
import Control.Exception (SomeException, fromException, throwIO, try)
import Data.Aeson (Value, encode, object, (.=))
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time (
    NominalDiffTime,
    UTCTime,
    addUTCTime,
    defaultTimeLocale,
    formatTime,
    parseTimeM,
 )
import Shared.Domain.Error (
    DomainError,
    createOperationNotAllowed,
    createServiceUnavailable,
    createUnexpectedError,
 )
import Shared.Domain.Identifier (ULID)
import Shared.UseCase.Command (Command (..))
import "media" Media.Domain.Image
import "media" Media.UseCase.InspectImage (InspectImageResult, foldInspectImageResult)
import "media" Media.UseCase.ProcessImageInspection
import "media" Media.UseCase.RetainImages

newInspectionDependencies ::
    IO (Either DomainError ULID) ->
    D1 ->
    ( ImageUploadDeclaration ->
      TemporaryObjectKey ->
      FinalObjectKey ->
      IO InspectionNormalization
    ) ->
    (TemporaryObjectKey -> IO ()) ->
    IO UTCTime ->
    InspectionDependencies
newInspectionDependencies generateIdentifier database normalize deleteTemporary getCurrentTime =
    InspectionDependencies
        { claimCurrentUpload = \attempt uploadedAt startedAt ->
            databaseOperation
                "claim current image upload"
                (claimCurrent database attempt uploadedAt startedAt)
        , normalizeImage = normalize
        , currentTime = getCurrentTime
        , commitInspection = \command normalization result key ->
            databaseOperation
                "commit image inspection"
                ( commitInspectionResult
                    generateIdentifier
                    database
                    command
                    normalization
                    result
                    key
                )
        , deleteTemporaryObject = deleteTemporary
        , persistInspectionFailure = \failure ->
            databaseOperation
                "persist image inspection failure"
                (persistFailure generateIdentifier database failure)
        }

newRetentionDependencies ::
    IO (Either DomainError ULID) ->
    D1 ->
    (RetentionObject -> IO ()) ->
    (Text -> IO ()) ->
    RetentionDependencies
newRetentionDependencies generateIdentifier database deleteStored purge =
    RetentionDependencies
        { claimRetentionCandidates = \now ->
            databaseOperation
                "claim image retention candidates"
                (claimRetention generateIdentifier database now)
        , deleteObject = deleteStored
        , purgePublicURL = purge
        , finalizeRetention = \identifier ->
            databaseOperation
                "finalize image retention"
                (finalizeRetentionClaim database identifier)
        }

newImageIdentifierResult ::
    IO (Either DomainError ULID) ->
    IO (Either DomainError ImageIdentifier)
newImageIdentifierResult generateIdentifier =
    identifierResult
        "ImageIdentifier"
        (fmap newImageIdentifier <$> generateIdentifier)

newUploadAttemptIdentifierResult ::
    IO (Either DomainError ULID) ->
    IO (Either DomainError UploadAttemptIdentifier)
newUploadAttemptIdentifierResult generateIdentifier =
    identifierResult
        "UploadAttemptIdentifier"
        (fmap newUploadAttemptIdentifier <$> generateIdentifier)

persistImageResult ::
    D1 ->
    NominalDiffTime ->
    Image ->
    IO (Either DomainError ())
persistImageResult database lifetime image =
    case image of
        AwaitingUpload awaiting -> persistAwaiting awaiting
        _ ->
            pure
                ( Left
                    ( createOperationNotAllowed
                        "PersistImage"
                        "the API upload workflow only persists awaiting images"
                    )
                )
  where
    persistAwaiting awaiting =
        databaseResult "persist awaiting image" $ do
            let (imageText, attemptText, _, requestedAt) =
                    awaitingValues awaiting
                expiresAt = addUTCTime lifetime requestedAt
                key = uploadObjectKey imageText attemptText
            current <- findImageState database (awaitingImageIdentifier awaiting)
            case current of
                Nothing -> persistNew database awaiting expiresAt key
                Just "awaiting_upload" ->
                    persistRetry database awaiting expiresAt key
                Just _ ->
                    throwIO
                        ( createOperationNotAllowed
                            "PersistImage"
                            "the current image state cannot start an upload attempt"
                        )

findImageResult ::
    D1 ->
    ImageIdentifier ->
    IO (Either DomainError (Maybe Image))
findImageResult database imageIdentifier =
    databaseResult
        "find image"
        ( d1QueryFirst
            database
            ( statement
                findImageSQL
                [D1Text (imageIdentifierText imageIdentifier)]
            )
            imageDecoder
        )

findAvailableImagesResult ::
    D1 ->
    [ImageIdentifier] ->
    IO (Either DomainError [ImageIdentifier])
findAvailableImagesResult _ [] = pure (Right [])
findAvailableImagesResult database identifiers =
    databaseResult "find available images" $
        d1Query
            database
            ( statement
                ( "SELECT identifier FROM images WHERE state='available' "
                    <> "AND identifier IN (SELECT value FROM json_each(?))"
                )
                [D1Text encoded]
            )
            (d1Column "identifier" imageIdentifierDecoder)
  where
    encoded = TextEncoding.decodeUtf8 $ LazyByteString.toStrict $ encode $
        map imageIdentifierText identifiers

persistNew :: D1 -> AwaitingUploadImage -> UTCTime -> TemporaryObjectKey -> IO ()
persistNew database awaiting expiresAt key = do
    let (imageText, attemptText, declaration, requestedAt) = awaitingValues awaiting
        now = timeText requestedAt
    _ <-
        d1ExecuteBatch
            database
            [ statement
                insertImageSQL
                [D1Text imageText, D1Text attemptText, D1Text now, D1Text now, D1Text now]
            , uploadAttemptStatement
                imageText
                attemptText
                declaration
                requestedAt
                expiresAt
                key
            ]
    pure ()

persistRetry :: D1 -> AwaitingUploadImage -> UTCTime -> TemporaryObjectKey -> IO ()
persistRetry database awaiting expiresAt key = do
    let (imageText, attemptText, declaration, requestedAt) = awaitingValues awaiting
        now = timeText requestedAt
    previous <-
        d1QueryFirst
            database
            ( statement
                findAwaitingAttemptSQL
                [D1Text imageText]
            )
            (d1Column "current_upload_attempt_identifier" d1Text)
    oldAttempt <- maybe (throwIO (userError "retry requires an awaiting image")) pure previous
    results <-
        d1ExecuteBatch
            database
            [ uploadAttemptStatement
                imageText
                attemptText
                declaration
                requestedAt
                expiresAt
                key
            , statement
                supersedeUploadAttemptSQL
                [D1Text now, D1Text oldAttempt]
            , statement
                updateRetriedImageSQL
                [D1Text attemptText, D1Text now, D1Text now, D1Text imageText, D1Text oldAttempt]
            ]
    ensureChanged "retry lost a concurrent update" (last results)

findImageState :: D1 -> ImageIdentifier -> IO (Maybe Text)
findImageState database imageIdentifier =
    d1QueryFirst
        database
        ( statement
            "SELECT state FROM images WHERE identifier=?"
            [D1Text (imageIdentifierText imageIdentifier)]
        )
        (d1Column "state" (d1Refine validState d1Text))

claimCurrent ::
    D1 -> UploadAttemptIdentifier -> UTCTime -> UTCTime -> IO (Maybe InspectionClaim)
claimCurrent database attempt uploadedAt startedAt = do
    let attemptText = uploadAttemptIdentifierText attempt
        uploaded = timeText uploadedAt
        started = timeText startedAt
    _ <-
        d1ExecuteBatch
            database
            [ statement
                recordUploadedAtSQL
                [D1Text uploaded, D1Text attemptText]
            , statement
                beginInspectionSQL
                [D1Text started, D1Text started, D1Text attemptText]
            ]
    d1QueryFirst database (statement claimQuery [D1Text attemptText]) claimDecoder
  where
    claimQuery = findInspectionClaimSQL

commitInspectionResult ::
    IO (Either DomainError ULID) ->
    D1 ->
    Command UploadAttemptIdentifier ->
    InspectionNormalization ->
    InspectImageResult ->
    FinalObjectKey ->
    IO ()
commitInspectionResult generateIdentifier database command normalization result finalKey = do
    eventIdentifier <-
        imageIdentifierText . newImageIdentifier
            <$> domainValue generateIdentifier
    inspectionIdentifier <-
        uploadAttemptIdentifierText . newUploadAttemptIdentifier
            <$> domainValue generateIdentifier
    let occurredAt = timeText command.timestamp
    statements <-
        foldInspectImageResult
            ( acceptedStatements
                inspectionIdentifier
                eventIdentifier
                occurredAt
                command.payload
                finalKey
                normalization
            )
            (rejectedStatements eventIdentifier occurredAt command.payload)
            result
    results <- d1ExecuteBatch database statements
    ensureChanged
        "inspection completion lost a concurrent update"
        (results !! completionStatementIndex)
  where
    completionStatementIndex = case normalization of
        ImageNormalized _ -> 1
        ImagePermanentlyRejected _ -> 0

acceptedStatements ::
    Text ->
    Text ->
    Text ->
    UploadAttemptIdentifier ->
    FinalObjectKey ->
    InspectionNormalization ->
    AvailableImage ->
    event ->
    IO [D1Statement]
acceptedStatements
    inspectionIdentifier
    eventIdentifier
    occurredAt
    attempt
    finalKey
    normalization
    available
    _ =
        foldAvailableImage build available
      where
        build imageIdentifier _ = do
            evidence <- case normalization of
                ImageNormalized value -> pure value
                ImagePermanentlyRejected _ ->
                    throwIO (userError "accepted inspection has no normalization evidence")
            let imageText = imageIdentifierText imageIdentifier
                payload = jsonText (object ["imageIdentifier" .= imageText])
                (format, width, height, byteSize, pixelCount) = inspectionValues evidence
            pure
                [ statement
                    insertInspectionSQL
                    [ D1Text inspectionIdentifier
                    , D1Text imageText
                    , D1Text (uploadAttemptIdentifierText attempt)
                    , D1Text format
                    , D1Integer width
                    , D1Integer height
                    , D1Integer byteSize
                    , D1Integer pixelCount
                    , D1Text (finalObjectKeyText finalKey)
                    , D1Text occurredAt
                    ]
                , statement
                    completeAvailableImageSQL
                    [ D1Text occurredAt
                    , D1Text occurredAt
                    , D1Text imageText
                    , D1Text (uploadAttemptIdentifierText attempt)
                    ]
                , outboxStatement
                    eventIdentifier
                    imageText
                    "ImageBecameAvailable"
                    payload
                    occurredAt
                ]

rejectedStatements ::
    Text -> Text -> UploadAttemptIdentifier -> RejectedImageUpload -> event -> IO [D1Statement]
rejectedStatements eventIdentifier occurredAt attempt rejected _ =
    foldRejectedImageUpload build rejected
  where
    build imageIdentifier reason _ = do
        let imageText = imageIdentifierText imageIdentifier
            reasonCode = rejectionText reason
            payload = jsonText (object ["imageIdentifier" .= imageText, "reason" .= reasonCode])
        pure
            [ statement
                completeRejectedImageSQL
                [ D1Text occurredAt
                , D1Text reasonCode
                , D1Text occurredAt
                , D1Text imageText
                , D1Text (uploadAttemptIdentifierText attempt)
                ]
            , outboxStatement eventIdentifier imageText "ImageUploadRejected" payload occurredAt
            ]

persistFailure ::
    IO (Either DomainError ULID) ->
    D1 ->
    InspectionFailureRecord ->
    IO ()
persistFailure generateIdentifier database failure = do
    identifier <-
        uploadAttemptIdentifierText . newUploadAttemptIdentifier
            <$> domainValue generateIdentifier
    imageIdentifier <-
        d1QueryFirst
            database
            ( statement
                "SELECT image_identifier FROM upload_attempts WHERE identifier=?"
                [D1Text (uploadAttemptIdentifierText failure.uploadAttempt)]
            )
            (d1Column "image_identifier" d1Text)
    imageText <-
        maybe
            (throwIO (userError "inspection failure references an unknown upload attempt"))
            pure
            imageIdentifier
    _ <-
        d1Execute
            database
            ( statement
                insertInspectionFailureSQL
                [ D1Text identifier
                , D1Text imageText
                , D1Text (uploadAttemptIdentifierText failure.uploadAttempt)
                , D1Text failure.code
                , maybe D1Null D1Text failure.detail
                , D1Text (timeText failure.failedAt)
                ]
            )
    pure ()

claimRetention ::
    IO (Either DomainError ULID) ->
    D1 ->
    UTCTime ->
    IO [RetentionCandidate]
claimRetention generateIdentifier database now = do
    claimant <-
        imageIdentifierText . newImageIdentifier
            <$> domainValue generateIdentifier
    rows <-
        d1Query
            database
            (statement retentionQuery (replicate 3 (D1Text (timeText now))))
            retentionRowDecoder
    concat <$> traverse (claimOne claimant) rows
  where
    retentionQuery = findRetentionCandidatesSQL
    claimOne claimant (imageIdentifier, objectKey, publicKey) = do
        inserted <-
            d1Execute
                database
                ( statement
                    claimRetentionCandidateSQL
                    [ D1Text (imageIdentifierText imageIdentifier)
                    , D1Text claimant
                    , D1Text (timeText now)
                    , D1Text (timeText now)
                    ]
                )
        pure $
            if changed inserted == 1
                then
                    [ RetentionCandidate
                        imageIdentifier
                        (maybe (TemporaryObject objectKey) (\key -> FinalObject key key) publicKey)
                    ]
                else []

finalizeRetentionClaim :: D1 -> ImageIdentifier -> IO ()
finalizeRetentionClaim database imageIdentifier = do
    let identifier = D1Text (imageIdentifierText imageIdentifier)
    _ <-
        d1ExecuteBatch
            database
            [ statement "DELETE FROM retention_claims WHERE image_identifier=?" [identifier]
            , statement "DELETE FROM images WHERE identifier=?" [identifier]
            ]
    pure ()

imageDecoder :: D1RowDecoder Image
imageDecoder = do
    identifier <- d1Column "identifier" imageIdentifierDecoder
    state <- d1Column "state" (d1Refine validState d1Text)
    case state of
        "awaiting_upload" -> AwaitingUpload <$> awaitingDecoder identifier
        "inspecting" -> do
            awaiting <- awaitingDecoder identifier
            attempt <-
                d1Column
                    "current_upload_attempt_identifier"
                    uploadAttemptDecoder
            startedAt <- d1Column "inspection_started_at" timeDecoder
            either
                (error . show)
                (pure . Inspecting)
                (beginImageInspection startedAt attempt awaiting)
        "available" ->
            Available . restoreAvailableImage identifier
                <$> d1Column "available_at" timeDecoder
        "rejected" ->
            Rejected
                <$> ( restoreRejectedImageUpload identifier
                        <$> d1Column "rejection_code" rejectionDecoder
                        <*> d1Column "rejected_at" timeDecoder
                    )
        _ -> error "validated image state became unreachable"

awaitingDecoder :: ImageIdentifier -> D1RowDecoder AwaitingUploadImage
awaitingDecoder identifier =
    newAwaitingUploadImage identifier
        <$> d1Column "current_upload_attempt_identifier" uploadAttemptDecoder
        <*> declarationDecoder
        <*> d1Column "requested_at" timeDecoder

declarationDecoder :: D1RowDecoder ImageUploadDeclaration
declarationDecoder =
    newImageUploadDeclaration
        <$> d1Column
            "declared_content_type"
            (d1Refine (firstError newDeclaredImageContentType) d1Text)
        <*> d1Column "declared_byte_size" (d1Refine (firstError newImageByteSize) d1Integer)
        <*> d1Column "declared_sha256" (d1Refine (firstError newImageSha256) d1Text)

claimDecoder :: D1RowDecoder InspectionClaim
claimDecoder = do
    identifier <- d1Column "image_identifier" imageIdentifierDecoder
    attempt <- d1Column "attempt_identifier" uploadAttemptDecoder
    declaration <- declarationDecoder
    requestedAt <- d1Column "requested_at" timeDecoder
    key <- newTemporaryObjectKey <$> d1Column "temporary_object_key" d1Text
    pure (InspectionClaim (newAwaitingUploadImage identifier attempt declaration requestedAt) key)

retentionRowDecoder :: D1RowDecoder (ImageIdentifier, Text, Maybe Text)
retentionRowDecoder =
    (,,)
        <$> d1Column "image_identifier" imageIdentifierDecoder
        <*> d1Column "object_key" d1Text
        <*> d1Column "public_key" (d1Nullable d1Text)

uploadAttemptStatement ::
    Text ->
    Text ->
    ImageUploadDeclaration ->
    UTCTime ->
    UTCTime ->
    TemporaryObjectKey ->
    D1Statement
uploadAttemptStatement imageText attemptText declaration requestedAt expiresAt key =
    foldImageUploadDeclaration build declaration
  where
    build contentType byteSize digest =
        statement
            insertUploadAttemptSQL
            [ D1Text attemptText
            , D1Text imageText
            , D1Text (temporaryObjectKeyText key)
            , D1Text (declaredImageContentTypeText contentType)
            , D1Integer (imageByteSizeInteger byteSize)
            , D1Text (imageSha256Text digest)
            , D1Text (timeText requestedAt)
            , D1Text (timeText expiresAt)
            ]

outboxStatement :: Text -> Text -> Text -> Text -> Text -> D1Statement
outboxStatement eventIdentifier aggregate eventType payload occurredAt =
    statement
        insertOutboxSQL
        [ D1Text eventIdentifier
        , D1Text aggregate
        , D1Text eventType
        , D1Text payload
        , D1Text eventIdentifier
        , D1Text occurredAt
        ]

statement :: Text -> [D1Value] -> D1Statement
statement = D1Statement

changed :: D1RunResult -> Int
changed = maybe 0 id . d1MetaChanges . d1RunResultMeta

ensureChanged :: String -> D1RunResult -> IO ()
ensureChanged message result
    | changed result == 1 = pure ()
    | otherwise = throwIO (userError message)

databaseResult :: Text -> IO value -> IO (Either DomainError value)
databaseResult operation action =
    first
        toDomainError
        <$> try @SomeException action
  where
    toDomainError exception =
        maybe
            ( createServiceUnavailable
                "MediaDatabase"
                (operation <> " failed")
            )
            id
            (fromException exception)

databaseOperation :: Text -> IO value -> IO value
databaseOperation operation action =
    domainValue (databaseResult operation action)

identifierResult ::
    Text ->
    IO (Either DomainError value) ->
    IO (Either DomainError value)
identifierResult identifierName action =
    flattenResult
        . first
            ( const
                ( createUnexpectedError
                    identifierName
                    "identifier generation failed"
                )
            )
        <$> try @SomeException action

flattenResult :: Either error (Either error value) -> Either error value
flattenResult = either Left id

domainValue :: IO (Either DomainError value) -> IO value
domainValue action = action >>= either throwIO pure

awaitingValues :: AwaitingUploadImage -> (Text, Text, ImageUploadDeclaration, UTCTime)
awaitingValues =
    foldAwaitingUploadImage
        ( \identifier attempt declaration requestedAt ->
            ( imageIdentifierText identifier
            , uploadAttemptIdentifierText attempt
            , declaration
            , requestedAt
            )
        )

awaitingImageIdentifier :: AwaitingUploadImage -> ImageIdentifier
awaitingImageIdentifier =
    foldAwaitingUploadImage (\identifier _ _ _ -> identifier)

uploadObjectKey :: Text -> Text -> TemporaryObjectKey
uploadObjectKey imageIdentifier uploadAttempt =
    newTemporaryObjectKey (imageIdentifier <> "/" <> uploadAttempt)

imageIdentifierDecoder :: D1ValueDecoder ImageIdentifier
imageIdentifierDecoder = d1Refine (firstError imageIdentifierFromText) d1Text

uploadAttemptDecoder :: D1ValueDecoder UploadAttemptIdentifier
uploadAttemptDecoder = d1Refine (firstError uploadAttemptIdentifierFromText) d1Text

timeDecoder :: D1ValueDecoder UTCTime
timeDecoder =
    d1Refine
        (\value -> maybe (Left "invalid UTC timestamp") Right (parseTime value))
        d1Text

validState :: Text -> Either Text Text
validState value
    | value `elem` ["awaiting_upload", "inspecting", "available", "rejected"] = Right value
    | otherwise = Left "unknown image state"

rejectionDecoder :: D1ValueDecoder ImageRejection
rejectionDecoder = d1Refine rejectionFromCode d1Text

rejectionFromCode :: Text -> Either Text ImageRejection
rejectionFromCode "unsupported_format" = Right UnsupportedImageFormat
rejectionFromCode "malformed" = Right MalformedImage
rejectionFromCode "byte_size_mismatch" = Right ImageByteSizeMismatch
rejectionFromCode "sha256_mismatch" = Right ImageSha256Mismatch
rejectionFromCode "sha256_missing" = Right ImageSha256Missing
rejectionFromCode value =
    Left
        ( "rejection details are unavailable for persisted code: "
            <> value
        )

rejectionText :: ImageRejection -> Text
rejectionText UnsupportedImageFormat = "unsupported_format"
rejectionText MalformedImage = "malformed"
rejectionText ImageByteSizeMismatch = "byte_size_mismatch"
rejectionText ImageSha256Mismatch = "sha256_mismatch"
rejectionText ImageSha256Missing = "sha256_missing"
rejectionText ImageFileTooLarge{} = "file_too_large"
rejectionText ImageDimensionsTooLarge{} = "dimensions_too_large"
rejectionText ImageHasTooManyPixels{} = "pixel_count_too_large"

timeText :: UTCTime -> Text
timeText = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

parseTime :: Text -> Maybe UTCTime
parseTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . Text.unpack

jsonText :: Value -> Text
jsonText = TextEncoding.decodeUtf8 . LazyByteString.toStrict . encode

firstError :: (Show error) => (input -> Either error output) -> input -> Either Text output
firstError refine input = either (Left . Text.pack . show) Right (refine input)

inspectionValues :: SuccessfulImageInspection -> (Text, Integer, Integer, Integer, Integer)
inspectionValues = foldSuccessfulImageInspection $ \format dimensions byteSize pixelCount _ _ _ _ ->
    let (width, height) =
            foldImageDimensions
                (\w h -> (imageWidthInteger w, imageHeightInteger h))
                dimensions
     in ( sourceFormatText format
        , width
        , height
        , imageByteSizeInteger byteSize
        , imagePixelCountInteger pixelCount
        )

insertImageSQL :: Text
insertImageSQL =
    sql
        [ "INSERT INTO images"
        , "(identifier,state,current_upload_attempt_identifier,requested_at,created_at,updated_at)"
        , "VALUES (?,'awaiting_upload',?,?,?,?)"
        ]

findAwaitingAttemptSQL :: Text
findAwaitingAttemptSQL =
    sql
        [ "SELECT current_upload_attempt_identifier FROM images"
        , "WHERE identifier=? AND state='awaiting_upload'"
        ]

supersedeUploadAttemptSQL :: Text
supersedeUploadAttemptSQL =
    sql
        [ "UPDATE upload_attempts SET superseded_at=?"
        , "WHERE identifier=? AND superseded_at IS NULL"
        ]

updateRetriedImageSQL :: Text
updateRetriedImageSQL =
    sql
        [ "UPDATE images"
        , "SET current_upload_attempt_identifier=?,requested_at=?,updated_at=?"
        , "WHERE identifier=? AND state='awaiting_upload'"
        , "AND current_upload_attempt_identifier=?"
        ]

findImageSQL :: Text
findImageSQL =
    sql
        [ "SELECT i.identifier,i.state,i.current_upload_attempt_identifier,"
        , "i.requested_at,i.inspection_started_at,i.available_at,i.rejected_at,"
        , "i.rejection_code,a.declared_content_type,a.declared_byte_size,"
        , "a.declared_sha256"
        , "FROM images i"
        , "LEFT JOIN upload_attempts a"
        , "ON a.identifier=i.current_upload_attempt_identifier"
        , "WHERE i.identifier=?"
        ]

beginInspectionSQL :: Text
beginInspectionSQL =
    sql
        [ "UPDATE images"
        , "SET state='inspecting',inspection_started_at=?,updated_at=?"
        , "WHERE state='awaiting_upload' AND current_upload_attempt_identifier=?"
        ]

recordUploadedAtSQL :: Text
recordUploadedAtSQL =
    sql
        [ "UPDATE upload_attempts SET uploaded_at=COALESCE(uploaded_at,?)"
        , "WHERE identifier=? AND superseded_at IS NULL"
        ]

findInspectionClaimSQL :: Text
findInspectionClaimSQL =
    sql
        [ "SELECT i.identifier AS image_identifier,a.identifier AS attempt_identifier,"
        , "a.declared_content_type,a.declared_byte_size,a.declared_sha256,"
        , "a.requested_at,a.temporary_object_key"
        , "FROM images i JOIN upload_attempts a"
        , "ON a.identifier=i.current_upload_attempt_identifier"
        , "WHERE i.state='inspecting' AND a.identifier=?"
        ]

insertInspectionSQL :: Text
insertInspectionSQL =
    sql
        [ "INSERT INTO image_inspections"
        , "(identifier,image_identifier,upload_attempt_identifier,source_format,width,height,"
        , "byte_size,pixel_count,final_object_key,inspected_at)"
        , "VALUES (?,?,?,?,?,?,?,?,?,?)"
        ]

completeAvailableImageSQL :: Text
completeAvailableImageSQL =
    sql
        [ "UPDATE images SET state='available',current_upload_attempt_identifier=NULL,"
        , "requested_at=NULL,inspection_started_at=NULL,available_at=?,updated_at=?"
        , "WHERE identifier=? AND state='inspecting'"
        , "AND current_upload_attempt_identifier=?"
        ]

completeRejectedImageSQL :: Text
completeRejectedImageSQL =
    sql
        [ "UPDATE images SET state='rejected',current_upload_attempt_identifier=NULL,"
        , "requested_at=NULL,inspection_started_at=NULL,rejected_at=?,rejection_code=?,"
        , "updated_at=? WHERE identifier=? AND state='inspecting'"
        , "AND current_upload_attempt_identifier=?"
        ]

insertInspectionFailureSQL :: Text
insertInspectionFailureSQL =
    sql
        [ "INSERT INTO inspection_failures"
        , "(identifier,image_identifier,upload_attempt_identifier,failure_code,detail,failed_at)"
        , "VALUES (?,?,?,?,?,?)"
        ]

findRetentionCandidatesSQL :: Text
findRetentionCandidatesSQL =
    sql
        [ "SELECT i.identifier AS image_identifier,"
        , "COALESCE(a.temporary_object_key,s.final_object_key) AS object_key,"
        , "s.final_object_key AS public_key FROM images i"
        , "LEFT JOIN upload_attempts a"
        , "ON a.identifier=i.current_upload_attempt_identifier"
        , "LEFT JOIN image_inspections s ON s.image_identifier=i.identifier"
        , "WHERE NOT EXISTS"
        , "(SELECT 1 FROM image_usages u WHERE u.image_identifier=i.identifier)"
        , "AND ((i.state='awaiting_upload'"
        , "AND datetime(i.requested_at)<=datetime(?,'-1 day'))"
        , "OR (i.state='rejected' AND datetime(i.rejected_at)<=datetime(?,'-7 day'))"
        , "OR (i.state='available' AND datetime(i.updated_at)<=datetime(?,'-30 day')))"
        , "LIMIT 100"
        ]

claimRetentionCandidateSQL :: Text
claimRetentionCandidateSQL =
    sql
        [ "INSERT OR IGNORE INTO retention_claims"
        , "(image_identifier,claimant_identifier,claimed_at,expires_at)"
        , "VALUES (?,?,?,strftime('%Y-%m-%dT%H:%M:%SZ',?,'+15 minutes'))"
        ]

insertUploadAttemptSQL :: Text
insertUploadAttemptSQL =
    sql
        [ "INSERT INTO upload_attempts"
        , "(identifier,image_identifier,temporary_object_key,declared_content_type,"
        , "declared_byte_size,declared_sha256,requested_at,expires_at)"
        , "VALUES (?,?,?,?,?,?,?,?)"
        ]

insertOutboxSQL :: Text
insertOutboxSQL =
    sql
        [ "INSERT INTO outbox"
        , "(identifier,aggregate_identifier,event_type,payload_json,idempotency_key,occurred_at)"
        , "VALUES (?,?,?,?,?,?)"
        ]

sql :: [Text] -> Text
sql = Text.unwords

sourceFormatText :: SourceImageFormat -> Text
sourceFormatText SourcePNG = "png"
sourceFormatText SourceJPEG = "jpeg"
sourceFormatText SourceWebP = "webp"
sourceFormatText SourceGIF = "gif"
sourceFormatText SourceHEIC = "heic"
