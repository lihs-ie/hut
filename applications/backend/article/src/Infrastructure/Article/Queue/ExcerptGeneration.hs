module Infrastructure.Article.Queue.ExcerptGeneration (
    GenerationRequestIdentifier,
    newGenerationRequestIdentifier,
    generationRequestIdentifierText,
    ExcerptGenerationRequested (..),
    ExcerptGenerationRequestedMessage (..),
    ExcerptGenerated (..),
    ExcerptGeneratedMessage (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time (UTCTime)
import "article" Domain.Article.Common (ArticleIdentifier, articleIdentifierText, newArticleIdentifier)
import "shared" Shared.Domain.Common.Primitive (newPositiveInteger)
import "shared" Shared.Domain.Error (DomainError)
import "shared" Shared.Domain.Excerpt (Excerpt, excerptText, newExcerpt)
import "shared" Shared.Domain.Identifier (ULID, newULID, ulidText)
import "shared" Shared.Infrastructure.Versioning (Version, newVersion, versionInteger)
import "shared" Shared.UseCase.Context (
    actorText,
    causationText,
    correlationIdentifierText,
    newActor,
    newCausation,
    newCorrelationIdentifier,
 )
import "shared" Shared.UseCase.Event (
    EventEnvelope (..),
    eventIdentifierText,
    newEventEnvelope,
    newEventIdentifier,
 )

newtype GenerationRequestIdentifier = GenerationRequestIdentifier ULID
    deriving stock (Show, Eq)

newGenerationRequestIdentifier :: Text -> Either DomainError GenerationRequestIdentifier
newGenerationRequestIdentifier = fmap GenerationRequestIdentifier . newULID

generationRequestIdentifierText :: GenerationRequestIdentifier -> Text
generationRequestIdentifierText (GenerationRequestIdentifier value) = ulidText value

data ExcerptGenerationRequested = ExcerptGenerationRequested
    { identifier :: GenerationRequestIdentifier
    , article :: ArticleIdentifier
    , expectedRevision :: Version
    }
    deriving stock (Show, Eq)

newtype ExcerptGenerationRequestedMessage = ExcerptGenerationRequestedMessage
    { envelope :: EventEnvelope ExcerptGenerationRequested
    }
    deriving stock (Show, Eq)

data ExcerptGenerated = ExcerptGenerated
    { request :: GenerationRequestIdentifier
    , article :: ArticleIdentifier
    , expectedRevision :: Version
    , excerpt :: Excerpt
    }
    deriving stock (Show, Eq)

newtype ExcerptGeneratedMessage = ExcerptGeneratedMessage
    { envelope :: EventEnvelope ExcerptGenerated
    }
    deriving stock (Show, Eq)

instance ToJSON ExcerptGenerationRequested where
    toJSON (ExcerptGenerationRequested identifier article expectedRevision) =
        object
            [ "identifier" .= generationRequestIdentifierText identifier
            , "article" .= articleIdentifierText article
            , "expectedRevision" .= versionInteger expectedRevision
            ]

instance FromJSON ExcerptGenerationRequested where
    parseJSON = withObject "ExcerptGenerationRequested" $ \value ->
        ExcerptGenerationRequested
            <$> (parseChecked . newGenerationRequestIdentifier =<< value .: "identifier")
            <*> (parseChecked . newArticleIdentifier =<< value .: "article")
            <*> (parseVersion =<< value .: "expectedRevision")

instance ToJSON ExcerptGenerated where
    toJSON (ExcerptGenerated request article expectedRevision excerpt) =
        object
            [ "request" .= generationRequestIdentifierText request
            , "article" .= articleIdentifierText article
            , "expectedRevision" .= versionInteger expectedRevision
            , "excerpt" .= excerptText excerpt
            ]

instance FromJSON ExcerptGenerated where
    parseJSON = withObject "ExcerptGenerated" $ \value ->
        ExcerptGenerated
            <$> (parseChecked . newGenerationRequestIdentifier =<< value .: "request")
            <*> (parseChecked . newArticleIdentifier =<< value .: "article")
            <*> (parseVersion =<< value .: "expectedRevision")
            <*> (parseChecked . newExcerpt =<< value .: "excerpt")

instance ToJSON ExcerptGenerationRequestedMessage where
    toJSON (ExcerptGenerationRequestedMessage value) = encodeEnvelope value

instance FromJSON ExcerptGenerationRequestedMessage where
    parseJSON = fmap ExcerptGenerationRequestedMessage . decodeEnvelope

instance ToJSON ExcerptGeneratedMessage where
    toJSON (ExcerptGeneratedMessage value) = encodeEnvelope value

instance FromJSON ExcerptGeneratedMessage where
    parseJSON = fmap ExcerptGeneratedMessage . decodeEnvelope

encodeEnvelope :: (ToJSON event) => EventEnvelope event -> Value
encodeEnvelope (EventEnvelope identifier occurredAt actor correlation causation event) =
    object
        [ "identifier" .= eventIdentifierText identifier
        , "occurredAt" .= occurredAt
        , "actor" .= actorText actor
        , "correlation" .= correlationIdentifierText correlation
        , "causation" .= fmap causationText causation
        , "event" .= event
        ]

decodeEnvelope :: (FromJSON event) => Value -> Parser (EventEnvelope event)
decodeEnvelope = withObject "EventEnvelope" $ \value -> do
    identifier <- parseChecked . newEventIdentifier =<< value .: "identifier"
    occurredAt <- value .: "occurredAt" :: Parser UTCTime
    actor <- parseChecked . newActor =<< value .: "actor"
    correlation <- parseChecked . newCorrelationIdentifier =<< value .: "correlation"
    causationTextValue <- value .: "causation"
    causation <- traverse (parseChecked . newCausation) causationTextValue
    event <- value .: "event"
    pure (newEventEnvelope identifier occurredAt actor correlation causation event)

parseVersion :: Integer -> Parser Version
parseVersion = fmap newVersion . parseChecked . newPositiveInteger

parseChecked :: (Show errorType) => Either errorType value -> Parser value
parseChecked = either (fail . show) pure
