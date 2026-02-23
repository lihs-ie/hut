{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import API.Server (api, server)
import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Lens ((&), (.~), (<&>))
import Data.ByteString.Char8 qualified as BS
import Gogol (envScopes, newEnv, override, serviceHost, servicePort, serviceSecure)
import Gogol.FireStore (CloudPlatform'FullControl, fireStoreService)
import Infrastructure.Firestore (createPersist, createTerminate)
import Network.Wai.Handler.Warp (run)
import Data.Proxy (Proxy (Proxy))
import Servant (serve)
import System.Envy (FromEnv (..), decodeEnv, env, envMaybe)

data AppConfig = AppConfig
  { projectIdentifier :: String,
    firestoreEmulatorHost :: Maybe String
  }

instance FromEnv AppConfig where
  fromEnv _ =
    AppConfig
      <$> env "FIREBASE_PROJECT_ID"
      <*> envMaybe "FIRESTORE_EMULATOR_HOST"

parseEmulatorHost :: String -> (BS.ByteString, Int)
parseEmulatorHost raw =
  let stripped = case drop 0 raw of
        ('h' : 't' : 't' : 'p' : ':' : '/' : '/' : rest) -> rest
        other -> other
      (host, portPart) = break (== ':') stripped
      port = case portPart of
        ':' : digits -> read digits
        _ -> 8085
   in (BS.pack host, port)

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) (pure ())
  result <- decodeEnv :: IO (Either String AppConfig)
  case result of
    Left message -> putStrLn ("Configuration error: " <> message)
    Right config -> do
      baseEnvironment <- newEnv <&> (envScopes .~ Proxy @'[CloudPlatform'FullControl])
      let environment = case config.firestoreEmulatorHost of
            Nothing -> baseEnvironment
            Just hostString ->
              let (host, port) = parseEmulatorHost hostString
                  emulatorConfig =
                    fireStoreService
                      & serviceHost .~ host
                      & servicePort .~ port
                      & serviceSecure .~ False
               in override emulatorConfig baseEnvironment
          persist = createPersist environment config.projectIdentifier
          terminate = createTerminate environment config.projectIdentifier
      putStrLn "Starting server on port 8080..."
      run 8080 (serve api (server persist terminate))
