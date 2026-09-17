module Media.Worker.API.Env (
    APIWorkerEnv,
) where

import Cloudflare.Workers.Binding.D1 (D1)
import Cloudflare.Workers.Binding.Queue (QueueProducer)
import Cloudflare.Workers.Env (BindingEnv)

type APIWorkerEnv =
    BindingEnv
        '[]
        '[]
        '[ '("MEDIA_DATABASE", D1)
         , '("MEDIA_INSPECTION_QUEUE", QueueProducer)
         ]
