module API.Handlers.Event (pubsubHandler, directEventHandler) where

import API.Request (EventRequest, PubSubPushRequest, decodePushRequest, toDomain)
import Aspects.Log (LogEntry)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Writer (WriterT (runWriterT))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Domain.SearchToken (Persist, TerminateByReference)
import Servant (Handler, ServerError (errBody), err400, err500, throwError)
import UseCase.EventHandler qualified as EventHandler

directEventHandler ::
  Persist (WriterT [LogEntry] IO) ->
  TerminateByReference (WriterT [LogEntry] IO) ->
  EventRequest ->
  Handler String
directEventHandler persist terminate request =
  case toDomain request of
    Left message ->
      throwError err400 {errBody = LBS.pack message}
    Right event -> do
      (result, logs) <- liftIO (runWriterT (EventHandler.handle persist terminate event))
      liftIO (mapM_ print logs)
      case result of
        Left _ -> throwError err500 {errBody = LBS.pack "Internal Server Error"}
        Right () -> pure "ok"

pubsubHandler ::
  Persist (WriterT [LogEntry] IO) ->
  TerminateByReference (WriterT [LogEntry] IO) ->
  PubSubPushRequest ->
  Handler String
pubsubHandler persist terminate request =
  case decodePushRequest request of
    Left message ->
      throwError err400 {errBody = LBS.pack message}
    Right eventRequest -> directEventHandler persist terminate eventRequest
