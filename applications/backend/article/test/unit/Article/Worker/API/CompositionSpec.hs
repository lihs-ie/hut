module Article.Worker.API.CompositionSpec (run) where

import Article.Worker.API.Composition (articleAPIHandler, isArticleAPIRoute)
import Cloudflare.Workers.Headers (headersFromList)
import Cloudflare.Workers.HTTP (Method (GET), Request (..), Response (..), Status (Status))
import Cloudflare.Workers.URL (parseURL)
import TestSupport (check)

run :: IO ()
run = do
    check "reader list route is forwarded" (isArticleAPIRoute "/articles")
    check "reader detail route is forwarded" (isArticleAPIRoute "/articles/haskell-syntax")
    check "admin list route is forwarded" (isArticleAPIRoute "/admin/articles")
    check "admin article route is forwarded" (isArticleAPIRoute "/admin/articles/01ARZ3NDEKTSV4RRFFQ69G5FAV")
    check "internal queue route is hidden" (not (isArticleAPIRoute "/internal/excerpt-generation/abandon"))
    check "route prefix cannot be confused" (not (isArticleAPIRoute "/articles-internal"))
    url <- maybe (fail "invalid test URL") pure $
        parseURL "https://article.internal/internal/excerpt-generation/abandon"
    let request = Request
            { requestMethodField = GET
            , requestURLField = url
            , requestBodyField = Nothing
            , requestHeaders = headersFromList []
            , requestBodyReaderField = Nothing
            , requestDataCenterField = Nothing
            }
    response <- articleAPIHandler request undefined undefined
    check "API worker rejects internal route before accessing bindings"
        (response.responseStatus == Status 404)
