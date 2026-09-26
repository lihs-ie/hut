module Main (main) where

import Article.Worker.Excerpt.Composition (excerptWorkerHandler)
import Cloudflare.Workers.Entrypoint.Queue (createQueueHandler)
import GHC.Wasm.Prim (JSVal)

queue :: JSVal -> JSVal -> JSVal -> IO ()
queue = createQueueHandler excerptWorkerHandler

foreign export javascript "queue" queue :: JSVal -> JSVal -> JSVal -> IO ()

main :: IO ()
main = pure ()
