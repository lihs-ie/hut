import { vi, type Mock } from "vitest";
import { ok, err, asyncResult } from "@shared/aspects/result";

/**
 * AsyncResult を処理する unwrapForNextJs のモック実装
 * AsyncResult.match() を呼び出し、成功時は値を返し、失敗時は例外をスローする
 */
async function unwrapAsyncResultImplementation<T>(asyncResultArgument: {
  match: <R>(handlers: {
    ok: (value: T) => R;
    err: (error: unknown) => R;
  }) => Promise<R>;
}): Promise<T> {
  return asyncResultArgument.match({
    ok: (value: T) => value,
    err: (error: unknown) => {
      throw error;
    },
  });
}

/**
 * AsyncResult を成功状態で作成する
 */
export function createSuccessAsyncResult<T>(value: T) {
  return asyncResult(Promise.resolve(ok(value)));
}

/**
 * AsyncResult を失敗状態で作成する
 */
export function createErrorAsyncResult<E>(error: E) {
  return asyncResult(Promise.resolve(err(error)));
}

/**
 * 標準的なアクションテスト用の beforeEach セットアップ
 * @param additionalMocks - 追加でリセットするモック関数の配列
 */
export function createStandardBeforeEach(...additionalMocks: Mock[]) {
  return () => {
    vi.clearAllMocks();
    vi.resetModules();
    for (const mock of additionalMocks) {
      mock.mockReset();
    }
  };
}

/**
 * unwrapForNextJs を AsyncResult 対応でセットアップする
 */
export async function setupUnwrapForAsyncResult() {
  const { unwrapForNextJs } =
    await import("@shared/components/global/next-error");
  vi.mocked(unwrapForNextJs).mockImplementation(
    unwrapAsyncResultImplementation,
  );
  return unwrapForNextJs;
}

/**
 * unwrapForNextJs を成功値でセットアップする
 */
export async function setupUnwrapWithResolvedValue<T>(value: T) {
  const { unwrapForNextJs } =
    await import("@shared/components/global/next-error");
  vi.mocked(unwrapForNextJs).mockResolvedValue(value);
  return unwrapForNextJs;
}

/**
 * unwrapForNextJs を失敗でセットアップする
 */
export async function setupUnwrapWithRejectedValue<E>(error: E) {
  const { unwrapForNextJs } =
    await import("@shared/components/global/next-error");
  vi.mocked(unwrapForNextJs).mockRejectedValue(error);
  return unwrapForNextJs;
}
