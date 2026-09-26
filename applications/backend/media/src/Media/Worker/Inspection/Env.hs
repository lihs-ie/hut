module Media.Worker.Inspection.Env (
    InspectionWorkerEnv,
) where

import Cloudflare.Workers.Binding.D1 (D1)
import Cloudflare.Workers.Binding.Images (Images)
import Cloudflare.Workers.Binding.R2 (R2Bucket)
import Cloudflare.Workers.Env (BindingEnv)

type InspectionWorkerEnv =
    BindingEnv
        '[]
        '[]
        '[ '("MEDIA_DATABASE", D1)
         , '("MEDIA_TMP_UPLOADS", R2Bucket)
         , '("MEDIA_ASSETS", R2Bucket)
         , '("IMAGES", Images)
         ]
