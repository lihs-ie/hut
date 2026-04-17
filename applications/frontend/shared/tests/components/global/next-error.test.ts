import { describe, it, expect, vi, beforeEach } from "vitest";
import { ok, err, asyncResult } from "@shared/aspects/result";

const captureExceptionMock = vi.fn();

vi.mock("@sentry/nextjs", () => ({
  captureException: (error: unknown, hint?: unknown) =>
    captureExceptionMock(error, hint),
}));

vi.mock("next/navigation", () => ({
  notFound: () => {
    throw new Error("NEXT_NOT_FOUND");
  },
}));

describe("components/global/next-error", () => {
  beforeEach(() => {
    captureExceptionMock.mockReset();
  });

  describe("unwrapForNextJs Sentry 連携", () => {
    it("Ok の AsyncResult を渡しても captureException は呼ばれない", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const success = asyncResult(Promise.resolve(ok<number, Error>(42)));
      const value = await unwrapForNextJs(success);

      expect(value).toBe(42);
      expect(captureExceptionMock).not.toHaveBeenCalled();
    });

    it("Err の AsyncResult を渡すと captureException が 1 度呼ばれる", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const failure = asyncResult(
        Promise.resolve(err<number, Error>(new Error("something failed"))),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).toHaveBeenCalledTimes(1);
    });

    it("captureException には元のエラー値が渡される", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const originalError = new Error("original");
      const failure = asyncResult(
        Promise.resolve(err<number, Error>(originalError)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).toHaveBeenCalledWith(
        originalError,
        expect.anything(),
      );
    });
  });
});
