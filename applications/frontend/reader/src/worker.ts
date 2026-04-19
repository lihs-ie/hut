// @ts-expect-error resolved by wrangler at build time
import openNextWorker from "../.open-next/worker.js";

// @ts-expect-error resolved by wrangler at build time
export { DOQueueHandler, DOShardedTagCache, BucketCachePurge } from "../.open-next/worker.js";

const INFORMATION_LEAK_HEADERS = [
  "X-Nextjs-Cache",
  "X-Nextjs-Prerender",
  "X-Nextjs-Stale-Time",
  "Server",
] as const;

type ReaderWorker = {
  fetch: (
    request: Request,
    env: CloudflareEnv,
    ctx: ExecutionContext,
  ) => Promise<Response>;
};

const handler: ReaderWorker = {
  async fetch(request, env, ctx) {
    const response = await (
      openNextWorker as ReaderWorker
    ).fetch(request, env, ctx);

    const headers = new Headers(response.headers);
    for (const header of INFORMATION_LEAK_HEADERS) {
      headers.delete(header);
    }

    return new Response(response.body, {
      status: response.status,
      statusText: response.statusText,
      headers,
    });
  },
};

export default handler;
