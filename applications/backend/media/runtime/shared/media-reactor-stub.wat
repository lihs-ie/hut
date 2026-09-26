;; This artifact exists solely to let Wrangler bundle a clean checkout.
;; It intentionally omits application exports; TypeScript rejects it before reactor initialization.
(module
  (memory (export "memory") 1)
  (func (export "_initialize"))
)
