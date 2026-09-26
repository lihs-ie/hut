module Presentation.Handler.API.Metadata (
    MetadataDependencies (..),
    newCommand,
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Presentation.Handler.API.Error (publicError)
import Servant.Cloudflare.Workers.Handler (Handler)
import Shared.Domain.Error (DomainError, createInvariantViolation)
import Shared.Domain.Identifier (newULID)
import Shared.UseCase.Command (
    Command (..),
    CorrelationIdentifier,
    newActor,
    newCorrelationIdentifier,
 )

data MetadataDependencies = MetadataDependencies
    { currentTime :: IO (Either DomainError UTCTime)
    , newCorrelation :: IO (Either DomainError Text)
    }

newCommand ::
    MetadataDependencies ->
    Maybe Text ->
    Maybe Text ->
    payload ->
    Handler env (Command payload, Text)
newCommand dependencies rawActor rawCorrelation payload = do
    (correlation, correlationText) <- resolveCorrelation dependencies rawCorrelation
    actor <- case rawActor >>= either (const Nothing) Just . newActor of
        Nothing -> throwError (publicError 400 "invalid_actor" correlationText)
        Just value -> pure value
    time <- liftIO dependencies.currentTime
    timestamp <- either
        (const (throwError (publicError 503 "timestamp_unavailable" correlationText)))
        pure
        time
    pure
        ( Command
            { payload
            , timestamp
            , actor
            , correlation
            , causation = Nothing
            }
        , correlationText
        )

resolveCorrelation ::
    MetadataDependencies ->
    Maybe Text ->
    Handler env (CorrelationIdentifier, Text)
resolveCorrelation dependencies supplied =
    case supplied of
        Just value ->
            either
                (const (throwError (publicError 400 "invalid_correlation" value)))
                pure
                (checkedCorrelation value)
        Nothing -> do
            generated <- liftIO dependencies.newCorrelation
            case generated >>= checkedCorrelation of
                Left _ -> throwError (publicError 503 "correlation_unavailable" emergencyCorrelation)
                Right value -> pure value

checkedCorrelation :: Text -> Either DomainError (CorrelationIdentifier, Text)
checkedCorrelation value = do
    _ <- newULID value
    correlation <- either
        (const (Left (createInvariantViolation "CorrelationIdentifier" "invalid correlation")))
        Right
        (newCorrelationIdentifier value)
    pure (correlation, value)

emergencyCorrelation :: Text
emergencyCorrelation = "00000000000000000000000000"
