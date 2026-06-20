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

type ExecBehavior =
  | { type: "resolve"; value: unknown }
  | { type: "reject"; error: Error };

const createPipelineStub = (execBehavior: ExecBehavior): PipelineStub => {
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

const createSequentialStubClient = (
  stubs: ReadonlyArray<PipelineStub>,
): { client: RedisPipelineClient; calls: () => number } => {
  let callCount = 0;
  const client: RedisPipelineClient = {
    pipeline: () => {
      const stub = stubs[callCount];
      callCount += 1;
      if (!stub) {
        throw new Error(
          `Unexpected pipeline() call #${callCount} (only ${stubs.length} stubs provided)`,
        );
      }
      return stub;
    },
  };
  return { client, calls: () => callCount };
};

const createStubClientFromBehaviors = (
  behaviors: ReadonlyArray<ExecBehavior>,
): { client: RedisPipelineClient; stubs: ReadonlyArray<PipelineStub> } => {
  const stubs = behaviors.map(createPipelineStub);
  const { client } = createSequentialStubClient(stubs);
  return { client, stubs };
};

describe("createUpstashRedisRateLimitStorage", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("初回 increment で count=1 を返し PEXPIRE を発行する", async () => {
    const { client, stubs } = createStubClientFromBehaviors([
      { type: "resolve", value: [1] },
      { type: "resolve", value: ["OK", -2] },
    ]);
    const storage = createUpstashRedisRateLimitStorage(client);
    const windowMs = 60000;

    const result = await storage.increment("ip:1.2.3.4", windowMs).match({
      ok: (window) => window,
      err: () => null,
    });

    expect(result).not.toBeNull();
    expect(result?.count).toBe(1);
    expect(stubs[1]?.pexpire).toHaveBeenCalledWith("ip:1.2.3.4", windowMs);
  });

  it("連続 increment で count が増加する場合 PEXPIRE を発行しない", async () => {
    const { client, stubs } = createStubClientFromBehaviors([
      { type: "resolve", value: [3] },
      { type: "resolve", value: [30000] },
    ]);
    const storage = createUpstashRedisRateLimitStorage(client);
    const windowMs = 60000;

    const result = await storage.increment("ip:1.2.3.4", windowMs).match({
      ok: (window) => window,
      err: () => null,
    });

    expect(result?.count).toBe(3);
    expect(stubs[1]?.pexpire).not.toHaveBeenCalled();
  });

  it("PTTL が正値の場合 resetAtMs は現在時刻 + PTTL で算出される", async () => {
    const remainingMs = 30000;
    const { client } = createStubClientFromBehaviors([
      { type: "resolve", value: [2] },
      { type: "resolve", value: [remainingMs] },
    ]);
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
    const { client } = createStubClientFromBehaviors([
      { type: "reject", error: new Error("connection refused") },
    ]);
    const storage = createUpstashRedisRateLimitStorage(client);

    const result = await storage.increment("ip:1.2.3.4", 60000).match({
      ok: () => null,
      err: (error) => error,
    });

    expect(result).not.toBeNull();
    expect(result?._tag).toBe(Symbol.for("ServiceUnavailableError"));
  });

  it("increment results が配列でない場合は ServiceUnavailableError を返す", async () => {
    const { client } = createStubClientFromBehaviors([
      { type: "resolve", value: null },
    ]);
    const storage = createUpstashRedisRateLimitStorage(client);

    const result = await storage.increment("ip:1.2.3.4", 60000).match({
      ok: () => null,
      err: (error) => error,
    });

    expect(result).not.toBeNull();
    expect(result?._tag).toBe(Symbol.for("ServiceUnavailableError"));
  });

  it("increment results の要素が不足している場合は ServiceUnavailableError を返す", async () => {
    const { client } = createStubClientFromBehaviors([
      { type: "resolve", value: [] },
    ]);
    const storage = createUpstashRedisRateLimitStorage(client);

    const result = await storage.increment("ip:1.2.3.4", 60000).match({
      ok: () => null,
      err: (error) => error,
    });

    expect(result).not.toBeNull();
    expect(result?._tag).toBe(Symbol.for("ServiceUnavailableError"));
  });

  it("ttl results の要素が不足している場合は ServiceUnavailableError を返す", async () => {
    const { client } = createStubClientFromBehaviors([
      { type: "resolve", value: [1] },
      { type: "resolve", value: [] },
    ]);
    const storage = createUpstashRedisRateLimitStorage(client);

    const result = await storage.increment("ip:1.2.3.4", 60000).match({
      ok: () => null,
      err: (error) => error,
    });

    expect(result).not.toBeNull();
    expect(result?._tag).toBe(Symbol.for("ServiceUnavailableError"));
  });
});
