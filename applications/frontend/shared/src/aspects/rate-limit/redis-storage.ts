import { Redis } from "@upstash/redis";
import { fromPromise } from "../result";
import { serviceUnavailableError } from "../error";
import type { RateLimitStorage } from "./storage";

export const createUpstashRedisRateLimitStorage = (
  client: Redis,
): RateLimitStorage => ({
  increment(key, windowMs) {
    return fromPromise(
      (async () => {
        const pipeline = client.pipeline();
        pipeline.incr(key);
        pipeline.pexpire(key, windowMs);
        pipeline.pttl(key);

        const results = await pipeline.exec();
        const count = results[0] as number;
        const remainingMs = results[2] as number;

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
