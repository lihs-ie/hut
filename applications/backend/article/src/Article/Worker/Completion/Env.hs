module Article.Worker.Completion.Env (
    CompletionWorkerEnv,
) where

import Cloudflare.Workers.Env (BindingEnv)
import Cloudflare.Workers.Binding.Var (Var)

type CompletionWorkerEnv =
    BindingEnv
        '[]
        '["ARTICLE_DO"]
        '[ '("COMPLETION_QUEUE_NAME", Var)
         , '("COMPLETION_DLQ_NAME", Var)
         ]
