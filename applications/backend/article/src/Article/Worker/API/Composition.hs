{-# LANGUAGE TypeApplications #-}

module Article.Worker.API.Composition (
    articleAPIHandler,
    isArticleAPIRoute,
) where

import Article.Worker.API.Env (ArticleAPIEnv)
import Cloudflare.Workers.Binding.DurableObject (doFetch, doGetByName)
import Cloudflare.Workers.Entrypoint.Fetch (FetchHandler)
import Cloudflare.Workers.Env (getDurableObjectNamespace)
import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (
    ResponseBody (ResponseBodyBytes),
    Status (Status),
    createResponse,
    requestPath,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Proxy (Proxy (Proxy))

articleAPIHandler :: FetchHandler ArticleAPIEnv
articleAPIHandler request environment _context
    | not (isArticleAPIRoute (requestPath request)) =
        pure (createResponse (Status 404) (headersFromList []) (ResponseBodyBytes ""))
    | otherwise = do
        stub <- doGetByName
            (getDurableObjectNamespace (Proxy @"ARTICLE_DO") environment)
            "articles"
        doFetch stub request

isArticleAPIRoute :: Text -> Bool
isArticleAPIRoute path = any matches ["/articles", "/admin/articles"]
  where
    matches prefix = path == prefix || (prefix <> "/") `Text.isPrefixOf` path
