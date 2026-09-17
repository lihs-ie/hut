module Media.Worker.ReferenceProjection.Env (
    ReferenceProjectionWorkerEnv,
) where

import Cloudflare.Workers.Binding.D1 (D1)
import Cloudflare.Workers.Env (BindingEnv)

type ReferenceProjectionWorkerEnv =
    BindingEnv
        '[]
        '[]
        '[ '("MEDIA_DATABASE", D1)]
