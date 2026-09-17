module Shared.UseCase.IdentifierSpec (run) where

import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Shared.Domain.Error (DomainError, createUnexpectedError)
import Shared.Domain.Identifier (ULID, newULID, ulidText)
import Shared.UseCase.Identifier (
    IdentifierGenerationDependencies (..),
    generateULID,
 )

run :: IO Bool
run = do
    generatedULIDResult <- generateULID successfulDependencies
    failedGenerationResult <- generateULID entropyFailureDependencies
    failedTimeGenerationResult <- generateULID timeFailureDependencies
    pure
        ( and
            [ generatedULIDResultIsCanonical generatedULIDResult
            , failedGenerationResult == Left entropyUnavailableError
            , failedTimeGenerationResult == Left timeUnavailableError
            ]
        )

successfulDependencies :: IdentifierGenerationDependencies
successfulDependencies =
    IdentifierGenerationDependencies
        { currentTime = pure (Right baseTime)
        , secureRandomBytes = \byteCount ->
            pure
                ( if byteCount == 10
                    then Right (ByteString.replicate byteCount 0xAB)
                    else Left unexpectedEntropyRequestError
                )
        }

entropyFailureDependencies :: IdentifierGenerationDependencies
entropyFailureDependencies =
    IdentifierGenerationDependencies
        { currentTime = pure (Right baseTime)
        , secureRandomBytes = \_ -> pure (Left entropyUnavailableError)
        }

timeFailureDependencies :: IdentifierGenerationDependencies
timeFailureDependencies =
    IdentifierGenerationDependencies
        { currentTime = pure (Left timeUnavailableError)
        , secureRandomBytes = \_ -> pure (Right (ByteString.replicate 10 0))
        }

generatedULIDResultIsCanonical :: Either DomainError ULID -> Bool
generatedULIDResultIsCanonical result =
    case result of
        Right identifier ->
            let value = ulidText identifier
             in Text.length value == 26
                    && newULID value == Right identifier
        Left _ -> False

unexpectedEntropyRequestError :: DomainError
unexpectedEntropyRequestError =
    createUnexpectedError
        "IdentifierGeneration"
        "the generator requested an unexpected entropy length"

entropyUnavailableError :: DomainError
entropyUnavailableError =
    createUnexpectedError
        "SecureRandom"
        "entropy source is unavailable"

timeUnavailableError :: DomainError
timeUnavailableError =
    createUnexpectedError
        "Clock"
        "current time is unavailable"

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2026 9 12) 0
