import { ok } from "../result";
import type { RateLimitStorage } from "./storage";

type StorageEntry = {
  count: number;
  windowStartMs: number;
};

type Options = {
  now?: () => Date;
};

export const createInMemoryRateLimitStorage = (
  options?: Options,
): RateLimitStorage => {
  const store = new Map<string, StorageEntry>();
  const nowFn = options?.now ?? (() => new Date());

  return {
    increment(key, windowMs) {
      const currentMs = nowFn().getTime();
      const existing = store.get(key);

      if (existing && currentMs - existing.windowStartMs < windowMs) {
        const nextCount = existing.count + 1;
        store.set(key, { count: nextCount, windowStartMs: existing.windowStartMs });
        return ok({
          count: nextCount,
          resetAtMs: existing.windowStartMs + windowMs,
        }).toAsync();
      }

      store.set(key, { count: 1, windowStartMs: currentMs });
      return ok({
        count: 1,
        resetAtMs: currentMs + windowMs,
      }).toAsync();
    },
  };
};
