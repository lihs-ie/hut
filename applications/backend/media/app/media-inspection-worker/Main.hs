module Main (main) where

import Cloudflare.Workers.Entrypoint.Queue (createQueueHandler)
import GHC.Wasm.Prim (JSVal)
import Media.Worker.Inspection.Composition (inspectionWorkerHandler)

queue :: JSVal -> JSVal -> JSVal -> IO ()
queue = createQueueHandler inspectionWorkerHandler

foreign export javascript "queue" queue :: JSVal -> JSVal -> JSVal -> IO ()

main :: IO ()
main = pure ()
