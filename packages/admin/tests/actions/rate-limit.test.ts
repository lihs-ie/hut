import { describe, it, expect, vi, beforeEach } from "vitest";
import { enforceLoginRateLimit, type RateLimitResult } from "@/actions/rate-limit";
import { ok, err } from "@shared/aspects/result";
import { resourceExhaustedError, unexpectedError } from "@shared/aspects/error";

vi.mock("@/aspects/rate-limit", () => ({
  enforceRateLimit: vi.fn(),
}));

vi.mock("@/providers/auth/admin", () => ({
  FirebaseAdminProvider: {
    firestore: {
      instance: {},
    },
  },
}));

const TEN_MINUTES_IN_MS = 10 * 60 * 1000;

describe("actions/rate-limit", () => {
  let mockEnforceRateLimit: ReturnType<typeof vi.fn>;

  beforeEach(async () => {
    vi.clearAllMocks();
    const rateLimitModule = await import("@/aspects/rate-limit");
    mockEnforceRateLimit = vi.mocked(rateLimitModule.enforceRateLimit);
  });

  describe("enforceLoginRateLimit", () => {
    describe("レート制限が許可される場合", () => {
      beforeEach(() => {
        mockEnforceRateLimit.mockReturnValue(ok(undefined).toAsync());
      });

      it("allowed: true を返す", async () => {
        const result = await enforceLoginRateLimit("test-key");

        expect(result).toEqual({ allowed: true });
      });

      it("正規化されたキーで enforceRateLimit を呼び出す", async () => {
        await enforceLoginRateLimit("user-ip-address");

        expect(mockEnforceRateLimit).toHaveBeenCalledWith(
          expect.anything(),
          expect.objectContaining({ key: "admin_login:user-ip-address" })
        );
      });

      it("正しいリミットとウィンドウで呼び出す", async () => {
        await enforceLoginRateLimit("test-key");

        expect(mockEnforceRateLimit).toHaveBeenCalledWith(
          expect.anything(),
          expect.objectContaining({
            limit: 5,
            windowMs: TEN_MINUTES_IN_MS,
          })
        );
      });
    });

    describe("ResourceExhaustedError の場合", () => {
      it("allowed: false とメッセージを返す", async () => {
        const errorMessage = "ログイン試行が多すぎます。しばらくしてから再試行してください。";
        mockEnforceRateLimit.mockReturnValue(
          err(resourceExhaustedError(errorMessage)).toAsync()
        );

        const result = await enforceLoginRateLimit("exceeded-key");

        expect(result).toEqual({ allowed: false, message: errorMessage });
      });

      it("カスタムエラーメッセージをそのまま返す", async () => {
        const customMessage = "カスタムエラーメッセージ";
        mockEnforceRateLimit.mockReturnValue(
          err(resourceExhaustedError(customMessage)).toAsync()
        );

        const result = await enforceLoginRateLimit("custom-error-key");

        expect(result).toEqual({ allowed: false, message: customMessage });
      });
    });

    describe("その他のエラーの場合", () => {
      it("UnexpectedError の場合は allowed: true を返す", async () => {
        mockEnforceRateLimit.mockReturnValue(
          err(unexpectedError("Internal error")).toAsync()
        );

        const result = await enforceLoginRateLimit("unexpected-error-key");

        expect(result).toEqual({ allowed: true });
      });
    });

    describe("キーの正規化", () => {
      beforeEach(() => {
        mockEnforceRateLimit.mockReturnValue(ok(undefined).toAsync());
      });

      const keyNormalizationCases = [
        { input: "  trimmed-key  ", expectedKey: "admin_login:trimmed-key" },
        { input: "", expectedKey: "admin_login:unknown" },
        { input: "   ", expectedKey: "admin_login:unknown" },
      ];

      keyNormalizationCases.forEach(({ input, expectedKey }) => {
        it(`"${input}" を "${expectedKey}" に正規化する`, async () => {
          await enforceLoginRateLimit(input);

          expect(mockEnforceRateLimit).toHaveBeenCalledWith(
            expect.anything(),
            expect.objectContaining({ key: expectedKey })
          );
        });
      });
    });

    describe("型チェック", () => {
      it("RateLimitResult 型の allowed: true を返す", async () => {
        mockEnforceRateLimit.mockReturnValue(ok(undefined).toAsync());

        const result: RateLimitResult = await enforceLoginRateLimit("type-check-key");

        expect(result.allowed).toBe(true);
        if (result.allowed === true) {
          expect(result).not.toHaveProperty("message");
        }
      });

      it("RateLimitResult 型の allowed: false を返す", async () => {
        mockEnforceRateLimit.mockReturnValue(
          err(resourceExhaustedError("Error message")).toAsync()
        );

        const result: RateLimitResult = await enforceLoginRateLimit("type-check-key-error");

        expect(result.allowed).toBe(false);
        if (result.allowed === false) {
          expect(result.message).toBe("Error message");
        }
      });
    });
  });
});
