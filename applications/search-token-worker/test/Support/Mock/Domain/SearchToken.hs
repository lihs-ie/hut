module Support.Mock.Domain.SearchToken
  ( createMockPersist,
    createMockTerminateByReference,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, modifyIORef')
import Domain.SearchToken (Persist, SearchToken, SearchTokenError, TerminateByReference)

createMockPersist :: (MonadIO m) => IORef [SearchToken] -> Either SearchTokenError () -> Persist m
createMockPersist record result tokens = do
  liftIO $ modifyIORef' record (<> tokens)
  pure result

createMockTerminateByReference :: (MonadIO m) => IORef [String] -> Either SearchTokenError () -> TerminateByReference m
createMockTerminateByReference record result reference = do
  liftIO $ modifyIORef' record (<> [reference])
  pure result
