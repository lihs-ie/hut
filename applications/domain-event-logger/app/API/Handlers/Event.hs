module API.Handlers.Event (pubsubHandler, directEventHandler) where

import API.Request (EventRequest, PubSubPushRequest, decodePushRequest, toDomain)
import Aspects.Log (LogEntry)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Writer (WriterT (runWriterT))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Servant (Handler, ServerError (errBody), err400, throwError)
import UseCase.EventLogger qualified as EventLogger

directEventHandler :: EventRequest -> Handler String
directEventHandler request =
  case toDomain request of
    Left message ->
      throwError err400 {errBody = LBS.pack message}
    Right event -> do
      (_, logs) <- liftIO (runWriterT (EventLogger.logEvent event) :: IO ((), [LogEntry]))
      liftIO (mapM_ print logs)
      pure "ok"

pubsubHandler :: PubSubPushRequest -> Handler String
pubsubHandler request =
  case decodePushRequest request of
    Left message ->
      throwError err400 {errBody = LBS.pack message}
    Right eventRequest -> directEventHandler eventRequest
