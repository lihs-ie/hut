module Main (main) where

import Cloudflare.Workers.Entrypoint.Fetch (createFetchHandler)
import GHC.Wasm.Prim (JSVal)
import Media.Infrastructure.Presign.Aws4Fetch (newAws4FetchPresigner)
import Media.Worker.API.Composition (apiWorkerHandler)

fetch :: JSVal -> JSVal -> JSVal -> IO JSVal
fetch request rawEnvironment context =
    createFetchHandler
        (apiWorkerHandler (newAws4FetchPresigner rawEnvironment))
        request
        rawEnvironment
        context

foreign export javascript "fetch" fetch :: JSVal -> JSVal -> JSVal -> IO JSVal

main :: IO ()
main = pure ()
