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
        const incrementPipeline = client.pipeline();
        incrementPipeline.incr(key);

        const incrementResults = await incrementPipeline.exec();

        if (!Array.isArray(incrementResults) || incrementResults.length < 1) {
          throw new Error(
            `Unexpected Redis pipeline result shape: ${JSON.stringify(incrementResults)}`,
          );
        }

        const count = toFiniteNumber(incrementResults[0], 0);

        const ttlPipeline = client.pipeline();
        if (count === 1) {
          ttlPipeline.pexpire(key, windowMs);
        }
        ttlPipeline.pttl(key);

        const ttlResults = await ttlPipeline.exec();
        const expectedLength = count === 1 ? 2 : 1;

        if (!Array.isArray(ttlResults) || ttlResults.length < expectedLength) {
          throw new Error(
            `Unexpected Redis pipeline result shape: ${JSON.stringify(ttlResults)}`,
          );
        }

        const remainingMs = toFiniteNumber(
          ttlResults[count === 1 ? 1 : 0],
          -1,
        );

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
