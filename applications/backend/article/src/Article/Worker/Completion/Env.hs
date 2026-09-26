module Article.Worker.Completion.Env (
    CompletionWorkerEnv,
) where

import Cloudflare.Workers.Env (BindingEnv)

type CompletionWorkerEnv = BindingEnv '[] '["ARTICLE_DO"] '[]
