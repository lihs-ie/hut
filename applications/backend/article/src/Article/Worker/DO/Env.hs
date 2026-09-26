module Article.Worker.DO.Env (
    ArticleDOEnv,
) where

import Cloudflare.Workers.Binding.DurableObject (DurableObjectStorage)
import Cloudflare.Workers.Binding.ServiceBinding (ServiceBinding)
import Cloudflare.Workers.Env (BindingEnv)

type ArticleDOEnv =
    BindingEnv
        '[]
        '[]
        '[ '("STORAGE", DurableObjectStorage)
         , '("MEDIA_API", ServiceBinding)
         ]
