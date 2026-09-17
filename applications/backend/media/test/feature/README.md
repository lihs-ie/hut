# Media feature tests

These tests execute the checked-in Media Worker runtime through Wrangler and observe it only at
HTTP, Queue, Scheduled, D1, R2, Images, and Service Binding boundaries. They do not import Haskell
modules or replace production dependencies.

The container has no external network at runtime. Consequently, a feature test cannot reach a
staging or production Cloudflare resource even if credentials exist on the host.

## Run

Build all four Haskell WASM executables and their JSFFI loaders into each runtime `generated`
directory, then run:

```sh
applications/backend/media/test/feature/run.sh
```

The harness refuses to run when any generated loader still identifies itself as a `stub`. Local
state is recreated for every run.

## Scenarios

- An external HTTP request reaches the API Worker through a Service Binding and is rejected by the
  real Access boundary when no token is supplied.
- D1 and R2 are exercised through the feature driver using their real Worker bindings.
- A valid PNG is stored in temporary R2 and delivered through a local Queue to the real inspection
  Worker. The test observes the resulting D1 state and R2 object movement.
- A reference event is delivered through a local Queue to the real projection Worker and observed
  in D1.
- An expired temporary image is seeded through the retention Worker's real D1 and R2 bindings. The
  real Scheduled handler is dispatched with Wrangler's Test Harness and deletion is observed
  through those bindings.
