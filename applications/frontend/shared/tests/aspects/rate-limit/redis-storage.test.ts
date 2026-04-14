/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import {
  createUpstashRedisRateLimitStorage,
  type PipelineCommands,
  type RedisPipelineClient,
} from "@shared/aspects/rate-limit/redis-storage";

type PipelineStub = PipelineCommands & {
  incr: ReturnType<typeof vi.fn>;
  pexpire: ReturnType<typeof vi.fn>;
  pttl: ReturnType<typeof vi.fn>;
  exec: ReturnType<typeof vi.fn>;
};

const createPipelineStub = (
  execBehavior:
    | { type: "resolve"; value: unknown }
    | { type: "reject"; error: Error },
): PipelineStub => {
  const stub: PipelineStub = {
    incr: vi.fn(() => stub),
    pexpire: vi.fn(() => stub),
    pttl: vi.fn(() => stub),
    exec:
      execBehavior.type === "resolve"
        ? vi.fn().mockResolvedValueOnce(execBehavior.value)
        : vi.fn().mockRejectedValueOnce(execBehavior.error),
  };

  return stub;
};

const createStubClient = (pipelineStub: PipelineStub): RedisPipelineClient => ({
  pipeline: () => pipelineStub,
});

describe("createUpstashRedisRateLimitStorage", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("初回 increment で count=1 を返す (PTTL=-2 の場合 windowMs をリセットAtMsとして算出)", async () => {
    const pipelineStub = createPipelineStub({
      type: "resolve",
      value: [1, "OK", -2],
    });
    const client = createStubClient(pipelineStub);
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
    const pipelineStub = createPipelineStub({
      type: "resolve",
      value: [3, null, 30000],
    });
    const client = createStubClient(pipelineStub);
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
    const pipelineStub = createPipelineStub({
      type: "resolve",
      value: [2, null, remainingMs],
    });
    const client = createStubClient(pipelineStub);
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
    const pipelineStub = createPipelineStub({
      type: "reject",
      error: new Error("connection refused"),
    });
    const client = createStubClient(pipelineStub);
    const storage = createUpstashRedisRateLimitStorage(client);

    const result = await storage.increment("ip:1.2.3.4", 60000).match({
      ok: () => null,
      err: (error) => error,
    });

    expect(result).not.toBeNull();
    expect(result?._tag).toBe(Symbol.for("ServiceUnavailableError"));
  });

  it("results が配列でない場合は ServiceUnavailableError を返す", async () => {
    const pipelineStub = createPipelineStub({
      type: "resolve",
      value: null,
    });
    const client = createStubClient(pipelineStub);
    const storage = createUpstashRedisRateLimitStorage(client);

    const result = await storage.increment("ip:1.2.3.4", 60000).match({
      ok: () => null,
      err: (error) => error,
    });

    expect(result).not.toBeNull();
    expect(result?._tag).toBe(Symbol.for("ServiceUnavailableError"));
  });

  it("results の要素が不足している場合は ServiceUnavailableError を返す", async () => {
    const pipelineStub = createPipelineStub({
      type: "resolve",
      value: [1],
    });
    const client = createStubClient(pipelineStub);
    const storage = createUpstashRedisRateLimitStorage(client);

    const result = await storage.increment("ip:1.2.3.4", 60000).match({
      ok: () => null,
      err: (error) => error,
    });

    expect(result).not.toBeNull();
    expect(result?._tag).toBe(Symbol.for("ServiceUnavailableError"));
  });
});
