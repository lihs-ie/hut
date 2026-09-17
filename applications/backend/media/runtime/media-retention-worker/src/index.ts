import {
  bindExport,
  createReactor,
  decodeVoid,
  defineWorker,
} from "@cloudflare-workers-hs/runtime";
import makeImports, { generatedArtifactKind } from "../generated/application-jsffi.mjs";
import wasmModule from "../generated/application.wasm";
import { rejectStubArtifact } from "../../shared/generated-artifact.js";

rejectStubArtifact("media-retention-worker", generatedArtifactKind);

const reactor = await createReactor(
  wasmModule,
  makeImports,
  (exports) => ({
    scheduled: bindExport<
      [
        event: ScheduledController,
        env: MediaRetentionWorkerEnv,
        context: ExecutionContext<unknown>,
      ],
      void
    >(exports, "scheduled", decodeVoid),
  }),
);

export default defineWorker({ scheduled: reactor.scheduled });
