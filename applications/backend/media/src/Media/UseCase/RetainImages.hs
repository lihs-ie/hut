module Media.UseCase.RetainImages (
    RetentionObject (..),
    RetentionCandidate (..),
    RetentionDependencies (..),
    RetentionResult (..),
    retainImages,
) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Time (UTCTime)
import Media.Domain.Image (ImageIdentifier)

data RetentionObject = TemporaryObject Text | FinalObject Text Text
    deriving stock (Show, Eq)

data RetentionCandidate = RetentionCandidate
    { image :: ImageIdentifier
    , object :: RetentionObject
    }
    deriving stock (Show, Eq)

data RetentionDependencies = RetentionDependencies
    { claimRetentionCandidates :: UTCTime -> IO [RetentionCandidate]
    , deleteObject :: RetentionObject -> IO ()
    , purgePublicURL :: Text -> IO ()
    , finalizeRetention :: ImageIdentifier -> IO ()
    }

newtype RetentionResult = RetentionResult {deletedCount :: Int}
    deriving stock (Show, Eq)

retainImages :: RetentionDependencies -> UTCTime -> IO RetentionResult
retainImages dependencies now = do
    candidates <- dependencies.claimRetentionCandidates now
    traverse_ deleteCandidate candidates
    pure (RetentionResult (length candidates))
  where
    deleteCandidate candidate = do
        dependencies.deleteObject candidate.object
        case candidate.object of
            TemporaryObject _ -> pure ()
            FinalObject _ publicURL -> dependencies.purgePublicURL publicURL
        dependencies.finalizeRetention candidate.image
