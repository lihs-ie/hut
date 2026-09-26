module Main (main) where

import Article.Worker.DO.Composition (articleDOHandler)
import Article.Worker.API.Composition (articleAPIHandler)
import Article.Worker.DO.Alarm (dispatchArticleOutboxAlarm)
import Cloudflare.Workers.Binding.DurableObject (DurableObjectStorage (..))
import Cloudflare.Workers.Binding.Queue (QueueProducer (..))
import Cloudflare.Workers.Entrypoint.Fetch (createFetchHandler)
import GHC.Wasm.Prim (JSVal)

fetch :: JSVal -> JSVal -> JSVal -> IO JSVal
fetch = createFetchHandler articleDOHandler

foreign export javascript "fetch" fetch :: JSVal -> JSVal -> JSVal -> IO JSVal

apiFetch :: JSVal -> JSVal -> JSVal -> IO JSVal
apiFetch = createFetchHandler articleAPIHandler

foreign export javascript "apiFetch" apiFetch :: JSVal -> JSVal -> JSVal -> IO JSVal

alarm :: JSVal -> JSVal -> JSVal -> IO ()
alarm storage generationQueue mediaQueue =
    dispatchArticleOutboxAlarm
        (DurableObjectStorage storage)
        (QueueProducer generationQueue)
        (QueueProducer mediaQueue)

foreign export javascript "alarm" alarm :: JSVal -> JSVal -> JSVal -> IO ()

main :: IO ()
main = pure ()
