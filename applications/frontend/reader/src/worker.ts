// @ts-expect-error resolved by wrangler at build time
import openNextWorker from "../.open-next/worker.js";

// @ts-expect-error resolved by wrangler at build time
export { DOQueueHandler, DOShardedTagCache, BucketCachePurge } from "../.open-next/worker.js";

import {
  isAllowedSearchBot,
  isMaliciousUserAgent,
  isStaticPath,
  resolveRateLimitKey,
} from "./security";

const INFORMATION_LEAK_HEADERS = [
  "X-Nextjs-Cache",
  "X-Nextjs-Prerender",
  "X-Nextjs-Stale-Time",
  "Server",
] as const;

type RateLimitBinding = {
  limit: (options: { key: string }) => Promise<{ success: boolean }>;
};

type ReaderEnv = CloudflareEnv & {
  RATE_LIMITER_GENERAL: RateLimitBinding;
  RATE_LIMITER_API: RateLimitBinding;
};

type ReaderWorker = {
  fetch: (
    request: Request,
    env: ReaderEnv,
    ctx: ExecutionContext,
  ) => Promise<Response>;
};

const stripLeakHeaders = (response: Response): Response => {
  const headers = new Headers(response.headers);
  for (const header of INFORMATION_LEAK_HEADERS) {
    headers.delete(header);
  }
  return new Response(response.body, {
    status: response.status,
    statusText: response.statusText,
    headers,
  });
};

const enforceAccessControl = async (
  request: Request,
  env: ReaderEnv,
): Promise<Response | null> => {
  const userAgent = request.headers.get("user-agent");

  if (isMaliciousUserAgent(userAgent)) {
    return new Response("Access denied", { status: 403 });
  }

  if (isAllowedSearchBot(userAgent)) {
    return null;
  }

  const url = new URL(request.url);
  if (isStaticPath(url.pathname)) {
    return null;
  }

  const limiter = url.pathname.startsWith("/api/")
    ? env.RATE_LIMITER_API
    : env.RATE_LIMITER_GENERAL;

  const { success } = await limiter.limit({
    key: resolveRateLimitKey(request, url.pathname),
  });

  if (!success) {
    return new Response("Rate limit exceeded", {
      status: 429,
      headers: { "Retry-After": "60" },
    });
  }

  return null;
};

const handler: ReaderWorker = {
  async fetch(request, env, ctx) {
    const blocked = await enforceAccessControl(request, env);
    if (blocked !== null) {
      return blocked;
    }

    const response = await (openNextWorker as ReaderWorker).fetch(
      request,
      env,
      ctx,
    );
    return stripLeakHeaders(response);
  },
};

export default handler;
