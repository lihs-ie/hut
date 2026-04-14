import { ok } from "../result";
import type { RateLimitStorage } from "./storage";

type StorageEntry = {
  count: number;
  windowStartMs: number;
  windowMs: number;
};

type Options = {
  now?: () => Date;
  maxEntries?: number;
};

const DEFAULT_MAX_ENTRIES = 10000;

const pruneExpired = (
  store: Map<string, StorageEntry>,
  currentMs: number,
): void => {
  for (const [key, entry] of store) {
    if (currentMs - entry.windowStartMs >= entry.windowMs) {
      store.delete(key);
    }
  }
};

const evictOldest = (store: Map<string, StorageEntry>): void => {
  const oldestKey = store.keys().next().value;
  if (oldestKey !== undefined) {
    store.delete(oldestKey);
  }
};

export const createInMemoryRateLimitStorage = (
  options?: Options,
): RateLimitStorage => {
  const store = new Map<string, StorageEntry>();
  const nowFunction = options?.now ?? (() => new Date());
  const maxEntries = options?.maxEntries ?? DEFAULT_MAX_ENTRIES;

  return {
    increment(key, windowMs) {
      const currentMs = nowFunction().getTime();
      const existing = store.get(key);

      if (existing && currentMs - existing.windowStartMs < windowMs) {
        const nextCount = existing.count + 1;
        store.set(key, {
          count: nextCount,
          windowStartMs: existing.windowStartMs,
          windowMs,
        });
        return ok({
          count: nextCount,
          resetAtMs: existing.windowStartMs + windowMs,
        }).toAsync();
      }

      if (store.size >= maxEntries) {
        pruneExpired(store, currentMs);
        if (store.size >= maxEntries) {
          evictOldest(store);
        }
      }

      store.set(key, {
        count: 1,
        windowStartMs: currentMs,
        windowMs,
      });
      return ok({
        count: 1,
        resetAtMs: currentMs + windowMs,
      }).toAsync();
    },
  };
};
