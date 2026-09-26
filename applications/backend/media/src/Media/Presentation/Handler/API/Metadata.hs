module Media.Presentation.Handler.API.Metadata (
    MetadataDependencies (..),
    canonicalCorrelation,
    generateCanonicalCorrelation,
    mapCommandPayload,
    newCommand,
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Media.Presentation.Handler.API.Error (
    invalidRequestError,
    publicServerError,
 )
import Servant.Cloudflare.Workers.Error (ServerError)
import Servant.Cloudflare.Workers.Handler (Handler)
import Shared.Domain.Error (DomainError, createInvariantViolation)
import Shared.Domain.Identifier (newULID)
import Shared.UseCase.Command (
    Command (Command),
    CorrelationIdentifier,
    newActor,
    newCorrelationIdentifier,
 )

data MetadataDependencies = MetadataDependencies
    { currentTime :: IO (Either DomainError UTCTime)
    , generateCorrelationIdentifier :: IO (Either DomainError Text)
    }

newCommand ::
    MetadataDependencies ->
    Maybe Text ->
    Maybe Text ->
    payload ->
    Handler env (Command payload, Text)
newCommand dependencies suppliedActor suppliedCorrelation payload = do
    (correlation, correlationText) <-
        resolveCorrelation dependencies suppliedCorrelation
    actor <-
        either
            ( const
                ( throwError
                    ( invalidRequestError
                        correlationText
                        "invalid_actor"
                    )
                )
            )
            pure
            ( maybe
                (Left invalidActorError)
                newActor
                suppliedActor
            )
    timestampResult <- liftIO dependencies.currentTime
    timestamp <-
        either
            (const (throwError (timestampGenerationError correlationText)))
            pure
            timestampResult
    pure
        ( Command
            payload
            timestamp
            actor
            correlation
            Nothing
        , correlationText
        )

resolveCorrelation ::
    MetadataDependencies ->
    Maybe Text ->
    Handler env (CorrelationIdentifier, Text)
resolveCorrelation dependencies suppliedCorrelation =
    case suppliedCorrelation of
        Just value -> case validatedCorrelation value of
            Right correlation -> pure correlation
            Left _ -> do
                (_, generated) <- generateCanonicalCorrelation dependencies
                throwError
                    ( invalidRequestError
                        generated
                        "invalid_correlation_identifier"
                    )
        Nothing -> generateCanonicalCorrelation dependencies

generateCanonicalCorrelation ::
    MetadataDependencies ->
    Handler env (CorrelationIdentifier, Text)
generateCanonicalCorrelation dependencies = do
    generated <- liftIO dependencies.generateCorrelationIdentifier
    case generated of
        Left domainError ->
            throwError (correlationGenerationError domainError)
        Right value ->
            either
                (const (throwError malformedGeneratedCorrelationError))
                pure
                (validatedCorrelation value)

canonicalCorrelation :: Text -> Maybe Text
canonicalCorrelation value
    | isCanonicalULID value = Just value
    | otherwise = Nothing

validatedCorrelation ::
    Text ->
    Either DomainError (CorrelationIdentifier, Text)
validatedCorrelation value
    | isCanonicalULID value =
        (,value)
            <$> mapLeft
                (const invalidCorrelationError)
                (newCorrelationIdentifier value)
    | otherwise = Left invalidCorrelationError

isCanonicalULID :: Text -> Bool
isCanonicalULID = either (const False) (const True) . newULID

invalidCorrelationError :: DomainError
invalidCorrelationError =
    createInvariantViolation
        "CorrelationIdentifier"
        "X-Correlation-Identifier must be a canonical ULID"

invalidActorError :: DomainError
invalidActorError =
    createInvariantViolation
        "Actor"
        "X-Hut-Actor is required"

correlationGenerationError :: DomainError -> ServerError
correlationGenerationError _ =
    publicServerError
        500
        "An unexpected error occurred."
        "correlation_generation_failed"
        emergencyCorrelation

malformedGeneratedCorrelationError :: ServerError
malformedGeneratedCorrelationError =
    publicServerError
        500
        "An unexpected error occurred."
        "correlation_generation_failed"
        emergencyCorrelation

timestampGenerationError :: Text -> ServerError
timestampGenerationError correlation =
    publicServerError
        500
        "An unexpected error occurred."
        "timestamp_generation_failed"
        correlation

emergencyCorrelation :: Text
emergencyCorrelation = "00000000000000000000000000"

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft transform = either (Left . transform) Right

mapCommandPayload :: (source -> target) -> Command source -> Command target
mapCommandPayload transform (Command payload timestamp actor correlation causation) =
    Command
        (transform payload)
        timestamp
        actor
        correlation
        causation
