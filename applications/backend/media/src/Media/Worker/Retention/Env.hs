module Media.Worker.Retention.Env (
    RetentionWorkerEnv,
) where

import Cloudflare.Workers.Binding.D1 (D1)
import Cloudflare.Workers.Binding.R2 (R2Bucket)
import Cloudflare.Workers.Binding.Secret (Secret)
import Cloudflare.Workers.Binding.Var (Var)
import Cloudflare.Workers.Env (BindingEnv)

type RetentionWorkerEnv =
    BindingEnv
        '[]
        '[]
        '[ '("MEDIA_DATABASE", D1)
         , '("MEDIA_TMP_UPLOADS", R2Bucket)
         , '("MEDIA_ASSETS", R2Bucket)
         , '("CLOUDFLARE_ZONE_IDENTIFIER", Var)
         , '("MEDIA_PUBLIC_BASE_URL", Var)
         , '("CLOUDFLARE_CACHE_PURGE_TOKEN", Secret)
         ]
