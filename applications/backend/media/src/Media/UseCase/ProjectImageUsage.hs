module Media.UseCase.ProjectImageUsage (
    ProjectionApplyResult (..),
    ImageUsageProjectionStore (..),
    projectImageUsage,
) where

import Data.Text (Text)
import Media.Domain.ImageUsage (ImageUsageProjection)

data ProjectionApplyResult = ProjectionApplied | ProjectionDuplicate | ProjectionOutOfOrder
    deriving stock (Show, Eq)

newtype ImageUsageProjectionStore = ImageUsageProjectionStore
    { replaceImageUsages :: Text -> ImageUsageProjection -> IO ProjectionApplyResult
    }

projectImageUsage ::
    ImageUsageProjectionStore -> Text -> ImageUsageProjection -> IO ProjectionApplyResult
projectImageUsage = (.replaceImageUsages)
