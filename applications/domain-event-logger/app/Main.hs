{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import API.Server (api, server)
import Network.Wai.Handler.Warp (run)
import Servant (serve)

main :: IO ()
main = do
  putStrLn "Starting server on port 8080..."
  run 8080 (serve api server)
