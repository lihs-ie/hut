module Main (main) where

import Cloudflare.Workers.Entrypoint.Scheduled (createScheduledHandler)
import GHC.Wasm.Prim (JSVal)
import Media.Worker.Retention.Composition (retentionWorkerHandler)

scheduled :: JSVal -> JSVal -> JSVal -> IO ()
scheduled = createScheduledHandler retentionWorkerHandler

foreign export javascript "scheduled" scheduled :: JSVal -> JSVal -> JSVal -> IO ()

main :: IO ()
main = pure ()
