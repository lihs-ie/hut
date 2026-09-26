module Main (main) where

import Article.Worker.Completion.Composition (completionWorkerHandler)
import Cloudflare.Workers.Entrypoint.Queue (createQueueHandler)
import GHC.Wasm.Prim (JSVal)

queue :: JSVal -> JSVal -> JSVal -> IO ()
queue = createQueueHandler completionWorkerHandler

foreign export javascript "queue" queue :: JSVal -> JSVal -> JSVal -> IO ()

main :: IO ()
main = pure ()
