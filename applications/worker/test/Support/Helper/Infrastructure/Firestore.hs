{-# LANGUAGE OverloadedStrings #-}

module Support.Helper.Infrastructure.Firestore
  ( createEmulatorEnvironment,
    resetFirestore,
    projectIdentifier,
  )
where

import Control.Lens ((&), (.~), (<&>))
import Data.Proxy (Proxy (..))
import Gogol (Env, envScopes, newEnv, override, serviceHost, servicePort, serviceSecure)
import Gogol.FireStore (CloudPlatform'FullControl, fireStoreService)
import Network.HTTP.Client (defaultManagerSettings, httpLbs, method, newManager, parseRequest)

projectIdentifier :: String
projectIdentifier = "demo-hut"

createEmulatorEnvironment :: IO (Env '[CloudPlatform'FullControl])
createEmulatorEnvironment = do
  baseEnvironment <- newEnv <&> (envScopes .~ Proxy @'[CloudPlatform'FullControl])
  let emulatorConfig =
        fireStoreService
          & serviceHost .~ "localhost"
          & servicePort .~ 8085
          & serviceSecure .~ False
  pure (override emulatorConfig baseEnvironment)

resetFirestore :: IO ()
resetFirestore = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest "http://localhost:8085/emulator/v1/projects/demo-hut/databases/(default)/documents"
  _ <- httpLbs (request {method = "DELETE"}) manager
  pure ()
