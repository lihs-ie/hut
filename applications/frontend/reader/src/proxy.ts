import { NextResponse } from "next/server";
import type { NextRequest } from "next/server";
import { Logger, Environment } from "@shared/aspects/logger";
import type { Logger as LoggerType } from "@shared/aspects/logger";
import {
  resolveIP,
  createInMemoryRateLimitStorage,
  createUpstashRedisRateLimitStorage,
  createRedisClientFromEnv,
  enforceRateLimit,
} from "@shared/aspects/rate-limit";
import type {
  RateLimitStorage,
  RateLimitDecision,
  RateLimitPolicy,
} from "@shared/aspects/rate-limit";
import type {
  ServiceUnavailableError,
  UnexpectedError,
} from "@shared/aspects/error";

const INFORMATION_LEAK_HEADERS = [
  "X-Nextjs-Cache",
  "X-Nextjs-Prerender",
  "X-Nextjs-Stale-Time",
  "Server",
] as const;

const DEFAULT_LIMIT = 100;
const DEFAULT_WINDOW_MS = 60000;

type EndpointKind = "search" | "feed" | "default";

const resolveEndpointKind = (pathname: string): EndpointKind => {
  if (pathname === "/search" || pathname.startsWith("/search/")) return "search";
  if (pathname === "/feed" || pathname.startsWith("/feed/")) return "feed";
  return "default";
};

const parseEnvironmentInteger = (
  value: string | undefined,
  fallback: number,
): number => {
  if (!value) return fallback;
  const parsed = parseInt(value, 10);
  return Number.isNaN(parsed) ? fallback : parsed;
};

const resolvePolicy = (kind: EndpointKind): RateLimitPolicy => {
  const prefix = `READER_RATE_LIMIT_${kind.toUpperCase()}`;
  return {
    limit: parseEnvironmentInteger(
      process.env[`${prefix}_LIMIT`],
      DEFAULT_LIMIT,
    ),
    windowMs: parseEnvironmentInteger(
      process.env[`${prefix}_WINDOW_MS`],
      DEFAULT_WINDOW_MS,
    ),
  };
};

const resolveAllowlist = (): ReadonlyArray<string> => {
  const raw = process.env.READER_RATE_LIMIT_ALLOWLIST;
  if (!raw) return [];
  return raw
    .split(",")
    .map((entry) => entry.trim())
    .filter((entry) => entry.length > 0)
    .map((entry) => `ip:${entry}`);
};

const isFailOpen = (): boolean =>
  process.env.READER_RATE_LIMIT_FAIL_OPEN === "true";

let cachedStorage: RateLimitStorage | null = null;

const getStorage = (): RateLimitStorage => {
  if (cachedStorage) return cachedStorage;

  if (process.env.UPSTASH_REDIS_REST_URL) {
    cachedStorage = createUpstashRedisRateLimitStorage(
      createRedisClientFromEnv(),
    );
  } else {
    cachedStorage = createInMemoryRateLimitStorage();
  }

  return cachedStorage;
};

const resolveEnvironment = (): Environment => {
  const value: string | undefined = process.env.NODE_ENV;
  if (value === Environment.PRODUCTION) return Environment.PRODUCTION;
  if (value === Environment.STAGING) return Environment.STAGING;
  return Environment.DEVELOPMENT;
};

const logger: LoggerType = Logger(resolveEnvironment());

const applyRateLimitHeaders = (
  response: NextResponse,
  decision: RateLimitDecision,
): void => {
  response.headers.set("X-RateLimit-Limit", String(decision.limit));
  response.headers.set("X-RateLimit-Remaining", String(decision.remaining));
  response.headers.set(
    "X-RateLimit-Reset",
    String(Math.ceil(decision.resetAtMs / 1000)),
  );
};

const buildRateLimitedResponse = (
  decision: RateLimitDecision,
): NextResponse => {
  const retryAfterSeconds = Math.max(
    1,
    Math.ceil((decision.resetAtMs - Date.now()) / 1000),
  );

  const response = NextResponse.json(
    { error: "Too Many Requests" },
    { status: 429 },
  );
  response.headers.set("Retry-After", String(retryAfterSeconds));
  applyRateLimitHeaders(response, decision);
  return response;
};

const stripInformationLeakHeaders = (response: NextResponse): void => {
  for (const header of INFORMATION_LEAK_HEADERS) {
    response.headers.delete(header);
  }
};

export async function proxy(request: NextRequest) {
  const kind = resolveEndpointKind(request.nextUrl.pathname);
  const policy = resolvePolicy(kind);
  const allowlist = resolveAllowlist();
  const failOpen = isFailOpen();
  const identifier = resolveIP(request);

  type RateLimitOutcome =
    | { kind: "ok"; decision: RateLimitDecision }
    | { kind: "err"; error: ServiceUnavailableError | UnexpectedError };

  const rateLimitOutcome: RateLimitOutcome = await enforceRateLimit(
    getStorage(),
    { ...policy, failOpen, allowlist },
    identifier,
  ).match<RateLimitOutcome>({
    ok: (decision) => ({ kind: "ok", decision }),
    err: (error) => ({ kind: "err", error }),
  });

  if (rateLimitOutcome.kind === "err") {
    logger.error("rate_limit_storage_unavailable", {
      identifier,
      endpoint: kind,
      limit: policy.limit,
      windowMs: policy.windowMs,
      error: rateLimitOutcome.error.message,
    });
    const errorResponse = new NextResponse(null, { status: 503 });
    stripInformationLeakHeaders(errorResponse);
    return errorResponse;
  }

  const decision = rateLimitOutcome.decision;

  if (!decision.allowed) {
    logger.warn("rate_limit_exceeded", {
      identifier,
      endpoint: kind,
      limit: decision.limit,
      count: decision.count,
      windowMs: policy.windowMs,
      resetAtMs: decision.resetAtMs,
    });
    const tooManyRequestsResponse = buildRateLimitedResponse(decision);
    stripInformationLeakHeaders(tooManyRequestsResponse);
    return tooManyRequestsResponse;
  }

  const response = NextResponse.next();
  applyRateLimitHeaders(response, decision);
  stripInformationLeakHeaders(response);
  return response;
}

export const config = {
  matcher: [
    { source: "/((?!api|_next/static|_next/image|favicon.ico).*)" },
  ],
  runtime: "nodejs",
};
