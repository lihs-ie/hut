{-# LANGUAGE TypeApplications #-}

module Article.Worker.API.Composition (
    articleAPIHandler,
) where

import Article.Worker.API.Env (ArticleAPIEnv)
import Cloudflare.Workers.Binding.DurableObject (doFetch, doGetByName)
import Cloudflare.Workers.Entrypoint.Fetch (FetchHandler)
import Cloudflare.Workers.Env (getDurableObjectNamespace)
import Data.Proxy (Proxy (Proxy))

articleAPIHandler :: FetchHandler ArticleAPIEnv
articleAPIHandler request environment _context = do
    stub <- doGetByName
        (getDurableObjectNamespace (Proxy @"ARTICLE_DO") environment)
        "articles"
    doFetch stub request
