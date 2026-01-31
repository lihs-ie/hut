/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { SearchReferenceIdentifierMold } from "../support/molds/domains/search-token/common";
import { ContentType } from "@shared/domains/search-token/reference";

const mockCookies = {
  get: vi.fn(),
  set: vi.fn(),
};

vi.mock("next/headers", () => ({
  cookies: vi.fn(() => Promise.resolve(mockCookies)),
}));

const mockTransaction = {
  get: vi.fn(),
  set: vi.fn(),
};

const mockFirestore = {
  instance: {},
  operations: {
    doc: vi.fn(),
    runTransaction: vi.fn((_, callback) => callback(mockTransaction)),
  },
};

vi.mock("@shared/providers/infrastructure/firebase", () => ({
  FirebaseProvider: {
    firestore: mockFirestore,
  },
}));

/**
 * Cookie取得のモック実装を作成する
 */
function createCookieGetMock(options: {
  adminSession?: string;
  pageViewSession?: string;
}) {
  return (name: string) => {
    if (name === "admin_session") {
      return options.adminSession ? { value: options.adminSession } : undefined;
    }
    if (name === "pv_session") {
      return options.pageViewSession ? { value: options.pageViewSession } : undefined;
    }
    return undefined;
  };
}

/**
 * Firestoreトランザクションのモック実装を作成する
 */
function setupTransactionMock(options: {
  isDuplicate: boolean;
  existingCount?: number;
}) {
  mockFirestore.operations.doc.mockReturnValue({});
  mockFirestore.operations.runTransaction.mockImplementation(
    async (_, callback) => {
      const dedupSnapshot = { exists: () => options.isDuplicate };
      mockTransaction.get.mockResolvedValueOnce(dedupSnapshot);

      if (!options.isDuplicate) {
        const counterSnapshot = {
          data: () =>
            options.existingCount !== undefined ? { count: options.existingCount } : undefined,
        };
        mockTransaction.get.mockResolvedValueOnce(counterSnapshot);
      }

      await callback(mockTransaction);
    }
  );
}

describe("view actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockCookies.get.mockReset();
    mockCookies.set.mockReset();
    mockTransaction.get.mockReset();
    mockTransaction.set.mockReset();
    mockFirestore.operations.doc.mockReset();
    mockFirestore.operations.runTransaction.mockReset();
  });

  describe("incrementViewCount", () => {
    it("初回訪問時にビューカウントをインクリメントする", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(1);
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      setupTransactionMock({ isDuplicate: false });

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);

      expect(mockCookies.set).toHaveBeenCalled();
      expect(mockTransaction.set).toHaveBeenCalled();
    });

    it("既存のセッションがある場合はcookieを設定しない", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(2);
      mockCookies.get.mockImplementation(
        createCookieGetMock({ pageViewSession: "existing-session-id" })
      );
      setupTransactionMock({ isDuplicate: false, existingCount: 5 });

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);

      expect(mockCookies.set).not.toHaveBeenCalled();
    });

    it("管理者セッションがある場合はカウントしない", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(3);
      mockCookies.get.mockImplementation(
        createCookieGetMock({ adminSession: "admin-session" })
      );

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);

      expect(mockFirestore.operations.runTransaction).not.toHaveBeenCalled();
    });

    it("同日の重複訪問はカウントしない", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(4);
      mockCookies.get.mockImplementation(
        createCookieGetMock({ pageViewSession: "existing-session-id" })
      );
      setupTransactionMock({ isDuplicate: true });

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);

      expect(mockTransaction.set).not.toHaveBeenCalled();
    });

    it("既存のカウントがある場合はインクリメントする", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(5);
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      setupTransactionMock({ isDuplicate: false, existingCount: 10 });

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);

      expect(mockTransaction.set).toHaveBeenCalled();
      const setCall = mockTransaction.set.mock.calls.find(
        (call) => call[1].count !== undefined
      );
      expect(setCall).toBeDefined();
      if (setCall !== undefined) {
        expect(setCall[1].count).toBe(11);
      }
    });

    it("記事タイプのidentifierを正しく処理する", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(6, {
        type: ContentType.ARTICLE,
      });
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      setupTransactionMock({ isDuplicate: false });

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);

      expect(mockFirestore.operations.doc).toHaveBeenCalled();
    });

    it("メモタイプのidentifierを正しく処理する", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(7, {
        type: ContentType.MEMO,
      });
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      setupTransactionMock({ isDuplicate: false });

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);

      expect(mockFirestore.operations.doc).toHaveBeenCalled();
    });

    it("シリーズタイプのidentifierを正しく処理する", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(8, {
        type: ContentType.SERIES,
      });
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      setupTransactionMock({ isDuplicate: false });

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);

      expect(mockFirestore.operations.doc).toHaveBeenCalled();
    });
  });
});
