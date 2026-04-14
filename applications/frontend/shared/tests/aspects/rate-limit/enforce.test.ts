/**
 * @vitest-environment node
 */
import { describe, it, expect } from "vitest";
import { ok, err } from "@shared/aspects/result";
import { serviceUnavailableError } from "@shared/aspects/error";
import { enforceRateLimit } from "@shared/aspects/rate-limit/enforce";
import type { RateLimitStorage } from "@shared/aspects/rate-limit/storage";

const createAllowedStorage = (count: number, resetAtMs: number): RateLimitStorage => ({
  increment: () => ok({ count, resetAtMs }).toAsync(),
});

const createStorageError = (): RateLimitStorage => ({
  increment: () =>
    err(serviceUnavailableError("storage error", true)).toAsync(),
});

describe("enforceRateLimit", () => {
  const basePolicy = {
    limit: 10,
    windowMs: 60000,
    failOpen: false,
  };

  it("リクエスト数が制限内の場合 allowed=true を返す", async () => {
    const resetAtMs = Date.now() + 60000;
    const storage = createAllowedStorage(5, resetAtMs);

    const result = await enforceRateLimit(storage, basePolicy, "ip:1.2.3.4").match({
      ok: (decision) => decision,
      err: () => null,
    });

    expect(result?.allowed).toBe(true);
    expect(result?.count).toBe(5);
    expect(result?.limit).toBe(10);
  });

  it("リクエスト数が制限を超えた場合 ResourceExhaustedError を返す", async () => {
    const resetAtMs = Date.now() + 60000;
    const storage = createAllowedStorage(11, resetAtMs);

    const result = await enforceRateLimit(storage, basePolicy, "ip:1.2.3.4").match({
      ok: () => null,
      err: (error) => error,
    });

    expect(result?._tag).toBe(Symbol.for("ResourceExhaustedError"));
  });

  it("ちょうど制限値の場合は allowed=true を返す", async () => {
    const resetAtMs = Date.now() + 60000;
    const storage = createAllowedStorage(10, resetAtMs);

    const result = await enforceRateLimit(storage, basePolicy, "ip:1.2.3.4").match({
      ok: (decision) => decision,
      err: () => null,
    });

    expect(result?.allowed).toBe(true);
  });

  describe("failOpen モード", () => {
    it("failOpen=true の場合はストレージエラー時に allowed=true を返す", async () => {
      const storage = createStorageError();
      const policy = { ...basePolicy, failOpen: true };

      const result = await enforceRateLimit(storage, policy, "ip:1.2.3.4").match({
        ok: (decision) => decision,
        err: () => null,
      });

      expect(result?.allowed).toBe(true);
    });

    it("failOpen=false の場合はストレージエラー時に ServiceUnavailableError を返す", async () => {
      const storage = createStorageError();
      const policy = { ...basePolicy, failOpen: false };

      const result = await enforceRateLimit(storage, policy, "ip:1.2.3.4").match({
        ok: () => null,
        err: (error) => error,
      });

      expect(result?._tag).toBe(Symbol.for("ServiceUnavailableError"));
    });
  });

  describe("allowlist", () => {
    it("allowlist に含まれる識別子は制限チェックをスキップして allowed=true を返す", async () => {
      const storage = createAllowedStorage(999, Date.now() + 60000);
      const policy = {
        ...basePolicy,
        limit: 1,
        allowlist: ["ip:1.2.3.4"],
      };

      const result = await enforceRateLimit(storage, policy, "ip:1.2.3.4").match({
        ok: (decision) => decision,
        err: () => null,
      });

      expect(result?.allowed).toBe(true);
    });

    it("allowlist に含まれない識別子は通常の制限チェックを受ける", async () => {
      const resetAtMs = Date.now() + 60000;
      const storage = createAllowedStorage(5, resetAtMs);
      const policy = {
        ...basePolicy,
        allowlist: ["ip:9.9.9.9"],
      };

      const result = await enforceRateLimit(storage, policy, "ip:1.2.3.4").match({
        ok: (decision) => decision,
        err: () => null,
      });

      expect(result?.allowed).toBe(true);
      expect(result?.count).toBe(5);
    });
  });

  it("RateLimitDecision に resetAtMs が含まれる", async () => {
    const resetAtMs = Date.now() + 60000;
    const storage = createAllowedStorage(1, resetAtMs);

    const result = await enforceRateLimit(storage, basePolicy, "ip:1.2.3.4").match({
      ok: (decision) => decision,
      err: () => null,
    });

    expect(result?.resetAtMs).toBe(resetAtMs);
  });
});
