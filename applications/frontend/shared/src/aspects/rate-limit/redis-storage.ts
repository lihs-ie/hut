import { Redis } from "@upstash/redis";
import { fromPromise } from "../result";
import { serviceUnavailableError } from "../error";
import type { RateLimitStorage } from "./storage";

export interface PipelineCommands {
  incr(key: string): PipelineCommands;
  pexpire(key: string, ms: number): PipelineCommands;
  pttl(key: string): PipelineCommands;
  exec(): Promise<unknown[]>;
}

export interface RedisPipelineClient {
  pipeline(): PipelineCommands;
}

const toFiniteNumber = (value: unknown, fallback: number): number => {
  if (typeof value === "number" && Number.isFinite(value)) {
    return value;
  }
  if (typeof value === "string") {
    const parsed = Number(value);
    if (Number.isFinite(parsed)) {
      return parsed;
    }
  }
  return fallback;
};

export const createUpstashRedisRateLimitStorage = (
  client: RedisPipelineClient,
): RateLimitStorage => ({
  increment(key, windowMs) {
    return fromPromise(
      (async () => {
        const pipeline = client.pipeline();
        pipeline.incr(key);
        pipeline.pexpire(key, windowMs);
        pipeline.pttl(key);

        const results = await pipeline.exec();

        if (!Array.isArray(results) || results.length < 3) {
          throw new Error(
            `Unexpected Redis pipeline result shape: ${JSON.stringify(results)}`,
          );
        }

        const count = toFiniteNumber(results[0], 0);
        const remainingMs = toFiniteNumber(results[2], -1);

        const nowMs = Date.now();
        const resetAtMs =
          remainingMs > 0 ? nowMs + remainingMs : nowMs + windowMs;

        return { count, resetAtMs };
      })(),
      (error) =>
        serviceUnavailableError(
          error instanceof Error
            ? error.message
            : "Redis rate limit storage error",
          true,
        ),
    );
  },
});

export const createRedisClientFromEnv = (): Redis =>
  new Redis({
    url: process.env.UPSTASH_REDIS_REST_URL ?? "",
    token: process.env.UPSTASH_REDIS_REST_TOKEN ?? "",
  });
