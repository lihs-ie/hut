import { describe, it, expect, vi, beforeEach } from "vitest";
import type { Firestore } from "firebase-admin/firestore";
import { enforceRateLimit, type RateLimitConfig } from "@/aspects/rate-limit";
import { isResourceExhaustedError, isUnexpectedError } from "@shared/aspects/error";

vi.mock("firebase-admin/firestore", () => ({
  Timestamp: {
    fromDate: (date: Date) => ({
      toDate: () => date,
      seconds: Math.floor(date.getTime() / 1000),
      nanoseconds: (date.getTime() % 1000) * 1000000,
    }),
  },
}));

interface RateLimitData {
  readonly count: number;
  readonly windowStart: Date;
}

interface MockTransaction {
  readonly get: ReturnType<typeof vi.fn>;
  readonly set: ReturnType<typeof vi.fn>;
}

interface MockFirestoreContext {
  readonly firestore: Firestore;
  readonly mockCollection: ReturnType<typeof vi.fn>;
  readonly mockRunTransaction: ReturnType<typeof vi.fn>;
  readonly transaction: MockTransaction;
  readonly docRef: ReturnType<typeof vi.fn>;
  snapshotData: RateLimitData | null;
  snapshotExists: boolean;
}

const createMockFirestoreContext = (): MockFirestoreContext => {
  const docRef = vi.fn().mockReturnValue({});
  let snapshotData: RateLimitData | null = null;
  let snapshotExists = false;

  const transaction: MockTransaction = {
    get: vi.fn().mockImplementation(() =>
      Promise.resolve({
        exists: snapshotExists,
        data: () => snapshotData,
      })
    ),
    set: vi.fn(),
  };

  const mockWithConverter = vi.fn().mockReturnValue({ doc: docRef });
  const mockCollectionResult = { withConverter: mockWithConverter, doc: docRef };
  const mockCollection = vi.fn().mockReturnValue(mockCollectionResult);
  const mockRunTransaction = vi.fn().mockImplementation(
    async <T>(callback: (tx: MockTransaction) => Promise<T>): Promise<T> => callback(transaction)
  );

  const firestore = vi.fn() as ReturnType<typeof vi.fn> & Firestore;
  firestore.collection = mockCollection;
  firestore.runTransaction = mockRunTransaction;

  const context: MockFirestoreContext = {
    firestore,
    mockCollection,
    mockRunTransaction,
    transaction,
    docRef,
    get snapshotData() { return snapshotData; },
    set snapshotData(value: RateLimitData | null) { snapshotData = value; },
    get snapshotExists() { return snapshotExists; },
    set snapshotExists(value: boolean) { snapshotExists = value; },
  };

  return context;
};

const createDefaultConfig = (key: string, limit = 5, windowMs = 60000): RateLimitConfig => ({
  key,
  limit,
  windowMs,
});

describe("aspects/rate-limit", () => {
  describe("enforceRateLimit", () => {
    let context: MockFirestoreContext;

    beforeEach(() => {
      vi.clearAllMocks();
      context = createMockFirestoreContext();
    });

    describe("新規リクエスト（レート制限ドキュメントが存在しない場合）", () => {
      it("最初のリクエストは許可されカウントを1に設定する", async () => {
        const config = createDefaultConfig("test-key");

        const result = await enforceRateLimit(context.firestore, config).unwrap();

        expect(result).toBeUndefined();
        expect(context.transaction.set).toHaveBeenCalledWith(
          expect.anything(),
          expect.objectContaining({ count: 1 }),
          { merge: true }
        );
      });
    });

    describe("既存のレート制限ドキュメントがある場合", () => {
      it("ウィンドウ内でリミット未満の場合はカウントを増加させる", async () => {
        const now = new Date();
        context.snapshotExists = true;
        context.snapshotData = {
          count: 2,
          windowStart: new Date(now.getTime() - 30000),
        };
        const config = createDefaultConfig("existing-key");

        const result = await enforceRateLimit(context.firestore, config).unwrap();

        expect(result).toBeUndefined();
        expect(context.transaction.set).toHaveBeenCalledWith(
          expect.anything(),
          expect.objectContaining({ count: 3 }),
          { merge: true }
        );
      });

      it("ウィンドウが期限切れの場合はカウントをリセットする", async () => {
        const now = new Date();
        context.snapshotExists = true;
        context.snapshotData = {
          count: 10,
          windowStart: new Date(now.getTime() - 120000),
        };
        const config = createDefaultConfig("expired-window-key");

        const result = await enforceRateLimit(context.firestore, config).unwrap();

        expect(result).toBeUndefined();
        expect(context.transaction.set).toHaveBeenCalledWith(
          expect.anything(),
          expect.objectContaining({ count: 1 }),
          { merge: true }
        );
      });

      it("リミットに達した場合は ResourceExhaustedError を返す", async () => {
        const now = new Date();
        context.snapshotExists = true;
        context.snapshotData = {
          count: 5,
          windowStart: new Date(now.getTime() - 30000),
        };
        const config = createDefaultConfig("limit-reached-key");

        const error = await enforceRateLimit(context.firestore, config).unwrapError();

        expect(isResourceExhaustedError(error)).toBe(true);
        expect(error.message).toBe("ログイン試行が多すぎます。しばらくしてから再試行してください。");
      });

      it("リミットを超えた場合は ResourceExhaustedError を返す", async () => {
        const now = new Date();
        context.snapshotExists = true;
        context.snapshotData = {
          count: 10,
          windowStart: new Date(now.getTime() - 30000),
        };
        const config = createDefaultConfig("over-limit-key");

        const error = await enforceRateLimit(context.firestore, config).unwrapError();

        expect(isResourceExhaustedError(error)).toBe(true);
      });
    });

    describe("エラーハンドリング", () => {
      it("トランザクションエラーを UnexpectedError にマッピングする", async () => {
        context.mockRunTransaction.mockRejectedValue(new Error("Transaction failed"));
        const config = createDefaultConfig("error-key");

        const error = await enforceRateLimit(context.firestore, config).unwrapError();

        expect(isUnexpectedError(error)).toBe(true);
        expect(error.message).toBe("Transaction failed");
      });

      it("非 Error オブジェクトのエラーもハンドリングする", async () => {
        context.mockRunTransaction.mockRejectedValue("String error");
        const config = createDefaultConfig("string-error-key");

        const error = await enforceRateLimit(context.firestore, config).unwrapError();

        expect(isUnexpectedError(error)).toBe(true);
        expect(error.message).toBe("Failed to apply rate limit");
      });
    });

    describe("コレクション名とドキュメント参照", () => {
      it("正しいコレクション名とキーを使用する", async () => {
        const config = createDefaultConfig("my-custom-key");

        await enforceRateLimit(context.firestore, config).unwrap();

        expect(context.mockCollection).toHaveBeenCalledWith("admin-rate-limits");
        expect(context.docRef).toHaveBeenCalledWith("my-custom-key");
      });
    });

    describe("境界値テスト", () => {
      it("リミットが1の場合、最初のリクエストは許可される", async () => {
        const config = createDefaultConfig("limit-1-key", 1);

        const result = await enforceRateLimit(context.firestore, config).unwrap();

        expect(result).toBeUndefined();
      });

      it("リミットが1で既に1回のリクエストがある場合は拒否される", async () => {
        const now = new Date();
        context.snapshotExists = true;
        context.snapshotData = {
          count: 1,
          windowStart: new Date(now.getTime() - 30000),
        };
        const config = createDefaultConfig("limit-1-exceeded-key", 1);

        const error = await enforceRateLimit(context.firestore, config).unwrapError();

        expect(isResourceExhaustedError(error)).toBe(true);
      });

      it("ウィンドウの境界で正しく動作する（ちょうど期限切れ）", async () => {
        const now = new Date();
        context.snapshotExists = true;
        context.snapshotData = {
          count: 5,
          windowStart: new Date(now.getTime() - 60001),
        };
        const config = createDefaultConfig("boundary-key");

        const result = await enforceRateLimit(context.firestore, config).unwrap();

        expect(result).toBeUndefined();
        expect(context.transaction.set).toHaveBeenCalledWith(
          expect.anything(),
          expect.objectContaining({ count: 1 }),
          { merge: true }
        );
      });
    });

    describe("データ存在チェック", () => {
      it("ドキュメントが存在するがデータがnullの場合は新規として扱う", async () => {
        context.snapshotExists = true;
        context.snapshotData = null;
        const config = createDefaultConfig("null-data-key");

        const result = await enforceRateLimit(context.firestore, config).unwrap();

        expect(result).toBeUndefined();
        expect(context.transaction.set).toHaveBeenCalledWith(
          expect.anything(),
          expect.objectContaining({ count: 1 }),
          { merge: true }
        );
      });
    });
  });
});
