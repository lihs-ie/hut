module Main (main) where

import Cloudflare.Workers.Entrypoint.Queue (createQueueHandler)
import GHC.Wasm.Prim (JSVal)
import Media.Worker.ReferenceProjection.Composition (
    referenceProjectionWorkerHandler,
 )

queue :: JSVal -> JSVal -> JSVal -> IO ()
queue = createQueueHandler referenceProjectionWorkerHandler

foreign export javascript "queue" queue :: JSVal -> JSVal -> JSVal -> IO ()

main :: IO ()
main = pure ()
