import {
  bindExport,
  createReactor,
  decodeVoid,
  defineWorker,
} from "@cloudflare-workers-hs/runtime";
import makeImports, { generatedArtifactKind } from "../generated/application-jsffi.mjs";
import wasmModule from "../generated/application.wasm";

if (generatedArtifactKind === "stub") {
  throw new Error("article-completion-worker requires a generated Haskell WASM artifact");
}

const reactor = await createReactor(
  wasmModule,
  makeImports,
  (exports) => ({
    queue: bindExport<
      [batch: unknown, env: unknown, context: unknown],
      void
    >(exports, "queue", decodeVoid),
  }),
);

export default defineWorker({ queue: reactor.queue });
