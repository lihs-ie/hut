import {
  bindExport,
  createReactor,
  decodeVoid,
  defineWorker,
} from "@cloudflare-workers-hs/runtime";
import makeImports, { generatedArtifactKind } from "../generated/application-jsffi.mjs";
import wasmModule from "../generated/application.wasm";
import { rejectStubArtifact } from "../../shared/generated-artifact.js";

rejectStubArtifact("media-inspection-worker", generatedArtifactKind);

const reactor = await createReactor(
  wasmModule,
  makeImports,
  (exports) => ({
    queue: bindExport<
      [
        batch: MessageBatch<unknown>,
        env: MediaInspectionWorkerEnv,
        context: ExecutionContext<unknown>,
      ],
      void
    >(exports, "queue", decodeVoid),
  }),
);

export default defineWorker({ queue: reactor.queue });
