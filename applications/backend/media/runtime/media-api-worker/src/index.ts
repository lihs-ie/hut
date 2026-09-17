import {
  bindExport,
  createReactor,
  decodeResponse,
  defineWorker,
} from "@cloudflare-workers-hs/runtime";
import makeImports, {
  generatedArtifactKind,
} from "../generated/application-jsffi.mjs";
import wasmModule from "../generated/application.wasm";
import { rejectStubArtifact } from "../../shared/generated-artifact.js";
import {
  createMediaApiAdapters,
  type MediaApiAdapters,
} from "./r2-presigner.js";

rejectStubArtifact("media-api-worker", generatedArtifactKind);

const reactor = await createReactor(
  wasmModule,
  makeImports,
  (exports) => ({
    fetch: bindExport<
      [
        request: Request,
        env: MediaApiPrivateJsffiEnvironment,
        context: ExecutionContext<unknown>,
      ],
      Response
    >(exports, "fetch", decodeResponse),
  }),
);

type MediaApiPrivateJsffiEnvironment = MediaApiWorkerEnv & {
  __mediaPresignR2Put: MediaApiAdapters["presignR2Put"];
};

async function fetch(
  request: Request,
  environment: MediaApiWorkerEnv,
  context: ExecutionContext<unknown>,
): Promise<Response> {
  const adapters = createMediaApiAdapters(environment);
  const jsffiEnvironment: MediaApiPrivateJsffiEnvironment = {
    ...environment,
    __mediaPresignR2Put: adapters.presignR2Put,
  };

  return reactor.fetch(request, jsffiEnvironment, context);
}

export default defineWorker({ fetch });
