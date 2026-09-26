import {
  bindExport,
  createReactor,
  decodeResponse,
  decodeVoid,
  defineWorker,
  initializeObject,
  runObject,
} from "@cloudflare-workers-hs/runtime";
import { DurableObject } from "cloudflare:workers";
import makeImports, {
  generatedArtifactKind,
} from "../generated/application-jsffi.mjs";
import wasmModule from "../generated/application.wasm";

if (generatedArtifactKind === "stub") {
  throw new Error("article-do-worker requires a generated Haskell WASM artifact");
}

const reactor = await createReactor(
  wasmModule,
  makeImports,
  (exports) => ({
    fetch: bindExport<
      [
        request: Request,
        env: {
          STORAGE: DurableObjectStorage;
          MEDIA_API: Fetcher;
          MEDIA_ASSET_ORIGIN?: string;
        },
        context: DurableObjectState,
      ],
      Response
    >(exports, "fetch", decodeResponse),
    apiFetch: bindExport<
      [
        request: Request,
        env: { ARTICLE_DO: DurableObjectNamespace },
        context: ExecutionContext<unknown>,
      ],
      Response
    >(exports, "apiFetch", decodeResponse),
    alarm: bindExport<
      [
        storage: DurableObjectStorage,
        generationQueue: Queue<unknown>,
        mediaQueue: Queue<unknown>,
      ],
      void
    >(exports, "alarm", decodeVoid),
    initialize: bindExport<
      [storage: DurableObjectStorage],
      void
    >(exports, "initialize", decodeVoid),
  }),
);

interface ArticleDOEnv {
  ARTICLE_EXCERPT_GENERATION_QUEUE: Queue<unknown>;
  ARTICLE_MEDIA_PROJECTION_QUEUE: Queue<unknown>;
  MEDIA_API: Fetcher;
  MEDIA_ASSET_ORIGIN?: string;
}

interface ArticleWorkerEnv extends ArticleDOEnv {
  ARTICLE_DO: DurableObjectNamespace;
}

export class ArticleDurableObject extends DurableObject<ArticleDOEnv> {
  constructor(context: DurableObjectState, environment: ArticleDOEnv) {
    super(context, environment);
    initializeObject(this.ctx, () => reactor.initialize(this.ctx.storage));
  }

  /** Invoke the Haskell fetch handler with this object's storage. */
  fetch(request: Request): Promise<Response> {
    return runObject(
      this.ctx,
      () =>
        reactor.fetch(
          request,
          {
            STORAGE: this.ctx.storage,
            MEDIA_API: this.env.MEDIA_API,
            MEDIA_ASSET_ORIGIN: this.env.MEDIA_ASSET_ORIGIN,
          },
          this.ctx,
        ),
      true,
    );
  }

  alarm(): Promise<void> {
    return runObject(
      this.ctx,
      () =>
        reactor.alarm(
          this.ctx.storage,
          this.env.ARTICLE_EXCERPT_GENERATION_QUEUE,
          this.env.ARTICLE_MEDIA_PROJECTION_QUEUE,
        ),
      true,
    );
  }
}

function fetch(
  request: Request,
  environment: ArticleWorkerEnv,
  context: ExecutionContext<unknown>,
): Promise<Response> {
  return reactor.apiFetch(request, { ARTICLE_DO: environment.ARTICLE_DO }, context);
}

export default defineWorker({ fetch });
