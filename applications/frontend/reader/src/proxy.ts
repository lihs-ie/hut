import { NextResponse } from "next/server";
import type { NextRequest } from "next/server";
import { Logger, Environment } from "@shared/aspects/logger";
import {
  resolveIP,
  createInMemoryRateLimitStorage,
  createUpstashRedisRateLimitStorage,
  createRedisClientFromEnv,
  enforceRateLimit,
} from "@shared/aspects/rate-limit";
import type { RateLimitStorage, RateLimitDecision } from "@shared/aspects/rate-limit";
import type {
  ResourceExhaustedError,
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
  if (pathname.startsWith("/search")) return "search";
  if (pathname.startsWith("/feed")) return "feed";
  return "default";
};

const parseEnvInt = (value: string | undefined, fallback: number): number => {
  if (!value) return fallback;
  const parsed = parseInt(value, 10);
  return Number.isNaN(parsed) ? fallback : parsed;
};

const resolvePolicy = (kind: EndpointKind) => {
  const prefix = `READER_RATE_LIMIT_${kind.toUpperCase()}`;
  return {
    limit: parseEnvInt(process.env[`${prefix}_LIMIT`], DEFAULT_LIMIT),
    windowMs: parseEnvInt(
      process.env[`${prefix}_WINDOW_MS`],
      DEFAULT_WINDOW_MS,
    ),
  };
};

const resolveAllowlist = (): string[] => {
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

let storage: RateLimitStorage | null = null;

const getStorage = (): RateLimitStorage => {
  if (storage) return storage;

  if (process.env.UPSTASH_REDIS_REST_URL) {
    storage = createUpstashRedisRateLimitStorage(createRedisClientFromEnv());
  } else {
    storage = createInMemoryRateLimitStorage();
  }

  return storage;
};

const logger = Logger(
  (process.env.NODE_ENV as Environment) ?? Environment.DEVELOPMENT,
);

export async function proxy(request: NextRequest) {
  const url = new URL(request.url);
  const kind = resolveEndpointKind(url.pathname);
  const policy = resolvePolicy(kind);
  const allowlist = resolveAllowlist();
  const failOpen = isFailOpen();
  const identifier = resolveIP(request);

  type MatchResult =
    | { type: "ok"; decision: RateLimitDecision }
    | {
        type: "err";
        error: ResourceExhaustedError | ServiceUnavailableError | UnexpectedError;
      };

  const rateLimitResult: MatchResult = await enforceRateLimit(
    getStorage(),
    { ...policy, failOpen, allowlist },
    identifier,
  ).match({
    ok: (decision): MatchResult => ({ type: "ok", decision }),
    err: (error): MatchResult => ({ type: "err", error }),
  });

  if (rateLimitResult.type === "err") {
    const error = rateLimitResult.error;

    if (error._tag === Symbol.for("ResourceExhaustedError")) {
      logger.warn("rate_limit_exceeded", {
        identifier,
        endpoint: kind,
        limit: policy.limit,
        windowMs: policy.windowMs,
      });

      const retryAfterSeconds = Math.ceil(policy.windowMs / 1000);
      const resetAtMs = Date.now() + policy.windowMs;

      const tooManyRequestsResponse = new NextResponse(
        JSON.stringify({ error: "Too Many Requests" }),
        { status: 429 },
      );
      tooManyRequestsResponse.headers.set(
        "Retry-After",
        String(retryAfterSeconds),
      );
      tooManyRequestsResponse.headers.set(
        "X-RateLimit-Limit",
        String(policy.limit),
      );
      tooManyRequestsResponse.headers.set(
        "X-RateLimit-Reset",
        String(Math.ceil(resetAtMs / 1000)),
      );
      return tooManyRequestsResponse;
    }

    if (!failOpen) {
      return new NextResponse(null, { status: 503 });
    }
  }

  const response = NextResponse.next();

  for (const header of INFORMATION_LEAK_HEADERS) {
    response.headers.delete(header);
  }

  return response;
}

export const config = {
  matcher: [
    { source: "/((?!api|_next/static|_next/image|favicon.ico).*)" },
  ],
};
