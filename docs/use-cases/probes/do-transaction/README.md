# Haskell WASM / DO SQLite transaction probe

This is a feasibility experiment, not the production Article implementation,
not a Docker/Wrangler feature test, and not an approved storage migration.
All database access and the illustrative decision run in Haskell. JavaScript
initializes the reactor, starts the platform transaction, translates failure
results into transaction aborts, and transports requests/responses.

## Environment Used

- wasm32-wasi-ghc: 9.14.1.20260731
- Miniflare: 4.20260617.1 (local workerd)
- Compatibility date: 2026-06-17
- Installed @cloudflare-workers-hs/runtime from hut media node_modules
- An independent WASM reactor per DO instance
- No remote bindings or production deployment

## Reproduce

From the repository root, with the GHC WASM tools on PATH:

```sh
mkdir -p _build/do-transaction-probe
wasm32-wasi-ghc docs/use-cases/probes/do-transaction/Main.hs \
  -o _build/do-transaction-probe/probe.wasm \
  -outputdir _build/do-transaction-probe \
  -no-hs-main -optl-mexec-model=reactor \
  -optl-Wl,--export=workflow \
  -optl-Wl,--export=initialize \
  -optl-Wl,--export=snapshot
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" \
  --input _build/do-transaction-probe/probe.wasm \
  --output _build/do-transaction-probe/probe-jsffi.mjs
node docs/use-cases/probes/do-transaction/bundle.cjs \
  "$ESBUILD_MODULE_PATH" "$RUNTIME_DIST_INDEX_PATH" _build/do-transaction-probe
node docs/use-cases/probes/do-transaction/check.cjs \
  "$MINIFLARE_MODULE_PATH" _build/do-transaction-probe/worker.mjs
```

Module path arguments are absolute paths to existing installations. No packages
are installed by these scripts. The check creates its own temporary storage,
disposes workerd, and removes that storage in finally. Compiled artifacts remain
in the ignored _build directory.

## Assertions

1. Read SQL row, make a Haskell decision, update, read own write, append Outbox.
2. Business rejection after the first write rolls back the article update.
3. Duplicate Outbox key rolls back the article and earlier Outbox insertion.
4. Haskell exception after both writes rolls back both changes.
5. Eight concurrent transactions with a Haskell async suspension produce eight
   distinct consecutive revisions, with exactly eight additional Outbox rows.
6. Concurrent successful and failing transactions preserve only successful work.
7. A second DO instance has independent state.
8. State survives disposal and recreation of the Miniflare runtime.

All assertions passed in the environment above.

## Important Boundaries

- Uses storage.transaction(async callback), NOT transactionSync.
- Haskell's normal return, including Left, does not itself abort the JS
  transaction. This probe encodes rejection as a negative integer; the JS bridge
  throws inside the callback. Production must preserve DomainError with a typed
  protocol instead of these status codes.
- SQL bindings and the illustrative domain model are probe-only. The real
  Article aggregate, full use cases, and a Haskell TransactionManager were not
  wired in this experiment.
- Current cloudflare-workers-hs doStorageTransaction takes a fixed KV operation
  list. A SQL API and a Haskell callback bridge are still needed for production.
- Timer suspension is only used to exercise concurrent requests. It is not a
  recommendation to include external services in a storage transaction.
- The finite concurrency test is not a proof for every possible schedule.
- Restart checks local persistence, not remote replication or disaster recovery.
- No benchmarks, crash-during-commit, network acknowledgement loss, WASM trap,
  CPU exhaustion, or production PITR tests were performed.
- A single Article DO is only a candidate. Its shared capacity and contention
  must be assessed before adopting it for all production reads and writes.
