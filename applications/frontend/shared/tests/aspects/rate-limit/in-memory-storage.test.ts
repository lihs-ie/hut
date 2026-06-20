/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { createInMemoryRateLimitStorage } from "@shared/aspects/rate-limit/in-memory-storage";

describe("createInMemoryRateLimitStorage", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("初回 increment で count=1 を返す", async () => {
    const storage = createInMemoryRateLimitStorage();

    const result = await storage.increment("key:1.2.3.4", 60000).match({
      ok: (window) => window,
      err: () => null,
    });

    expect(result).not.toBeNull();
    expect(result?.count).toBe(1);
  });

  it("同じキーで連続 increment するとカウントが増加する", async () => {
    const storage = createInMemoryRateLimitStorage();

    await storage.increment("key:1.2.3.4", 60000).match({
      ok: () => {},
      err: () => {},
    });
    await storage.increment("key:1.2.3.4", 60000).match({
      ok: () => {},
      err: () => {},
    });
    const result = await storage.increment("key:1.2.3.4", 60000).match({
      ok: (window) => window,
      err: () => null,
    });

    expect(result?.count).toBe(3);
  });

  it("windowMs 経過後は count がリセットされる", async () => {
    const storage = createInMemoryRateLimitStorage();
    const windowMs = 60000;

    await storage.increment("key:1.2.3.4", windowMs).match({
      ok: () => {},
      err: () => {},
    });

    vi.advanceTimersByTime(windowMs + 1);

    const result = await storage.increment("key:1.2.3.4", windowMs).match({
      ok: (window) => window,
      err: () => null,
    });

    expect(result?.count).toBe(1);
  });

  it("異なるキーは独立して管理される", async () => {
    const storage = createInMemoryRateLimitStorage();

    await storage.increment("key:1.2.3.4", 60000).match({
      ok: () => {},
      err: () => {},
    });
    await storage.increment("key:1.2.3.4", 60000).match({
      ok: () => {},
      err: () => {},
    });

    const result = await storage.increment("key:5.6.7.8", 60000).match({
      ok: (window) => window,
      err: () => null,
    });

    expect(result?.count).toBe(1);
  });

  it("resetAtMs は windowMs 後の時刻を返す", async () => {
    const now = new Date("2024-01-01T00:00:00Z");
    vi.setSystemTime(now);

    const storage = createInMemoryRateLimitStorage();
    const windowMs = 60000;

    const result = await storage.increment("key:1.2.3.4", windowMs).match({
      ok: (window) => window,
      err: () => null,
    });

    expect(result?.resetAtMs).toBe(now.getTime() + windowMs);
  });
});
