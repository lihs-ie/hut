module Article.Worker.API.Env (
    ArticleAPIEnv,
) where

import Cloudflare.Workers.Env (BindingEnv)

type ArticleAPIEnv =
    BindingEnv
        '[]
        '["ARTICLE_DO"]
        '[]
