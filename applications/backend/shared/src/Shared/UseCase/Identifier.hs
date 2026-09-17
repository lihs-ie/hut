module Shared.UseCase.Identifier (
    IdentifierGenerationDependencies (..),
    generateULID,
) where

import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Shared.Domain.Error (DomainError)
import Shared.Domain.Identifier (ULID, newULIDFromParts)

data IdentifierGenerationDependencies = IdentifierGenerationDependencies
    { currentTime :: IO (Either DomainError UTCTime)
    , secureRandomBytes :: Int -> IO (Either DomainError ByteString)
    }

generateULID ::
    IdentifierGenerationDependencies ->
    IO (Either DomainError ULID)
generateULID dependencies = do
    timestampResult <- dependencies.currentTime
    case timestampResult of
        Left domainError -> pure (Left domainError)
        Right timestamp -> do
            entropyResult <- dependencies.secureRandomBytes entropyLength
            pure (entropyResult >>= newULIDFromParts timestamp)

entropyLength :: Int
entropyLength = 10
