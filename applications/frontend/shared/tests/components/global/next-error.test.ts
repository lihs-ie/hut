import { describe, it, expect, vi, beforeEach } from "vitest";
import { ok, err, asyncResult } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  duplicationError,
  permissionDeniedError,
  resourceExhaustedError,
  serviceUnavailableError,
  unauthenticatedError,
  unexpectedError,
  validationError,
} from "@shared/aspects/error";

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

    it("AggregateNotFoundError（404）の場合は captureException が呼ばれない", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const notFoundError = aggregateNotFoundError("Stock", "not found");
      const failure = asyncResult(
        Promise.resolve(err<number, typeof notFoundError>(notFoundError)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).not.toHaveBeenCalled();
    });

    it("AggregateNotFoundError 配列の場合も captureException が呼ばれない", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const notFoundErrors = [
        aggregateNotFoundError("Stock", "not found"),
        aggregateNotFoundError("Application", "not found"),
      ];
      const failure = asyncResult(
        Promise.resolve(err<number, typeof notFoundErrors>(notFoundErrors)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).not.toHaveBeenCalled();
    });

    it("ValidationError（400）の場合は captureException が呼ばれない", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const error = validationError("email", "invalid format");
      const failure = asyncResult(
        Promise.resolve(err<number, typeof error>(error)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).not.toHaveBeenCalled();
    });

    it("ValidationError 配列の場合も captureException が呼ばれない", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const errors = [
        validationError("email", "invalid format"),
        validationError("name", "required"),
      ];
      const failure = asyncResult(
        Promise.resolve(err<number, typeof errors>(errors)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).not.toHaveBeenCalled();
    });

    it("UnauthenticatedError（401）の場合は captureException が呼ばれない", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const error = unauthenticatedError("login required");
      const failure = asyncResult(
        Promise.resolve(err<number, typeof error>(error)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).not.toHaveBeenCalled();
    });

    it("PermissionDeniedError（403）の場合は captureException が呼ばれない", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const error = permissionDeniedError("forbidden");
      const failure = asyncResult(
        Promise.resolve(err<number, typeof error>(error)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).not.toHaveBeenCalled();
    });

    it("DuplicationError（409）の場合は captureException が呼ばれない", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const error = duplicationError("Stock", "already exists");
      const failure = asyncResult(
        Promise.resolve(err<number, typeof error>(error)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).not.toHaveBeenCalled();
    });

    it("ResourceExhaustedError（429）の場合は captureException が呼ばれない", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const error = resourceExhaustedError("rate limit");
      const failure = asyncResult(
        Promise.resolve(err<number, typeof error>(error)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).not.toHaveBeenCalled();
    });

    it("ServiceUnavailableError（503）の場合は captureException が呼ばれない", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const error = serviceUnavailableError("temporarily down", true);
      const failure = asyncResult(
        Promise.resolve(err<number, typeof error>(error)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).not.toHaveBeenCalled();
    });

    it("UnexpectedError（想定外のドメインエラー）の場合は captureException が呼ばれる", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const error = unexpectedError("unknown failure");
      const failure = asyncResult(
        Promise.resolve(err<number, typeof error>(error)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).toHaveBeenCalledTimes(1);
      expect(captureExceptionMock).toHaveBeenCalledWith(
        error,
        expect.anything(),
      );
    });

    it("未知のエラー（UnexpectedHttpError へ落ちるケース）でも captureException が呼ばれる", async () => {
      const { unwrapForNextJs } = await import(
        "@shared/components/global/next-error"
      );

      const originalError = new Error("something failed");
      const failure = asyncResult(
        Promise.resolve(err<number, Error>(originalError)),
      );

      await expect(unwrapForNextJs(failure)).rejects.toThrow();
      expect(captureExceptionMock).toHaveBeenCalledTimes(1);
      expect(captureExceptionMock).toHaveBeenCalledWith(
        originalError,
        expect.anything(),
      );
    });
  });
});
