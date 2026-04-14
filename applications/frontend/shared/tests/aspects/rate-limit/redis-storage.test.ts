/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { createUpstashRedisRateLimitStorage } from "@shared/aspects/rate-limit/redis-storage";
import type { Redis } from "@upstash/redis";

const createMockClient = (execResult: unknown) => {
  const mockExec = vi.fn().mockResolvedValueOnce(execResult);
  const mockPipeline = {
    incr: vi.fn().mockReturnThis(),
    pexpire: vi.fn().mockReturnThis(),
    pttl: vi.fn().mockReturnThis(),
    exec: mockExec,
  };

  return {
    client: { pipeline: () => mockPipeline } as unknown as Redis,
    mockExec,
  };
};

const createFailingMockClient = (error: Error) => {
  const mockExec = vi.fn().mockRejectedValueOnce(error);
  const mockPipeline = {
    incr: vi.fn().mockReturnThis(),
    pexpire: vi.fn().mockReturnThis(),
    pttl: vi.fn().mockReturnThis(),
    exec: mockExec,
  };

  return {
    client: { pipeline: () => mockPipeline } as unknown as Redis,
    mockExec,
  };
};

describe("createUpstashRedisRateLimitStorage", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("初回 increment で count=1 を返す (PTTL=-2 の場合 windowMs をリセットAtMsとして算出)", async () => {
    const { client } = createMockClient([1, "OK", -2]);
    const storage = createUpstashRedisRateLimitStorage(client);
    const windowMs = 60000;

    const result = await storage.increment("ip:1.2.3.4", windowMs).match({
      ok: (window) => window,
      err: () => null,
    });

    expect(result).not.toBeNull();
    expect(result?.count).toBe(1);
  });

  it("連続 increment でカウントが増加する", async () => {
    const { client } = createMockClient([3, null, 30000]);
    const storage = createUpstashRedisRateLimitStorage(client);
    const windowMs = 60000;

    const result = await storage.increment("ip:1.2.3.4", windowMs).match({
      ok: (window) => window,
      err: () => null,
    });

    expect(result?.count).toBe(3);
  });

  it("PTTL が正値の場合 resetAtMs は現在時刻 + PTTL で算出される", async () => {
    const remainingMs = 30000;
    const { client } = createMockClient([2, null, remainingMs]);
    const beforeMs = Date.now();

    const storage = createUpstashRedisRateLimitStorage(client);

    const result = await storage.increment("ip:1.2.3.4", 60000).match({
      ok: (window) => window,
      err: () => null,
    });

    const afterMs = Date.now();

    expect(result?.resetAtMs).toBeGreaterThanOrEqual(beforeMs + remainingMs);
    expect(result?.resetAtMs).toBeLessThanOrEqual(afterMs + remainingMs);
  });

  it("Redis pipeline が例外を投げた場合 ServiceUnavailableError を返す", async () => {
    const { client } = createFailingMockClient(new Error("connection refused"));
    const storage = createUpstashRedisRateLimitStorage(client);

    const result = await storage.increment("ip:1.2.3.4", 60000).match({
      ok: () => null,
      err: (error) => error,
    });

    expect(result).not.toBeNull();
    expect(result?._tag).toBe(Symbol.for("ServiceUnavailableError"));
  });
});
