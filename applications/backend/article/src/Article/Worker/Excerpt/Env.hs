module Article.Worker.Excerpt.Env (
    ExcerptWorkerEnv,
) where

import Cloudflare.Workers.Binding.Queue (QueueProducer)
import Cloudflare.Workers.Binding.Var (Var)
import Cloudflare.Workers.Binding.WorkersAI (WorkersAI)
import Cloudflare.Workers.Env (BindingEnv)

type ExcerptWorkerEnv =
    BindingEnv
        '[]
        '["ARTICLE_DO"]
        '[ '("AI", WorkersAI)
         , '("ARTICLE_EXCERPT_COMPLETION_QUEUE", QueueProducer)
         , '("GENERATION_QUEUE_NAME", Var)
         , '("GENERATION_DLQ_NAME", Var)
         ]
