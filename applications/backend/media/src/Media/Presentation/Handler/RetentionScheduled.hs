module Media.Presentation.Handler.RetentionScheduled (
    RetentionHandlerDependencies (..),
    mediaRetentionScheduledHandler,
) where

import Cloudflare.Workers.Entrypoint.Scheduled
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import "media" Media.UseCase.RetainImages (RetentionDependencies, retainImages)

newtype RetentionHandlerDependencies = RetentionHandlerDependencies
    { retention :: RetentionDependencies
    }

mediaRetentionScheduledHandler :: ScheduledHandler RetentionHandlerDependencies
mediaRetentionScheduledHandler = handleScheduled

handleScheduled :: ScheduledHandler RetentionHandlerDependencies
handleScheduled controller dependencies _ = do
    let now =
            posixSecondsToUTCTime
                (fromInteger controller.scheduledControllerScheduledTime / 1000)
    _ <- retainImages dependencies.retention now
    pure ()
