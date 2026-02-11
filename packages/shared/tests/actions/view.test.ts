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

const mockHeaders = {
  get: vi.fn(),
};

vi.mock("next/headers", () => ({
  cookies: vi.fn(() => Promise.resolve(mockCookies)),
  headers: vi.fn(() => Promise.resolve(mockHeaders)),
}));

const pendingAfterCallbacks: Array<Promise<void>> = [];

vi.mock("next/server", () => ({
  after: (callback: () => Promise<void>) => {
    pendingAfterCallbacks.push(callback());
  },
}));

const flushAfterCallbacks = async () => {
  await Promise.all(pendingAfterCallbacks);
  pendingAfterCallbacks.length = 0;
};

const mockPageViewRecord = vi.fn();
const mockUniqueVisitorRecord = vi.fn();

const createMockAsyncResult = () => ({
  tapError: vi.fn().mockReturnValue(Promise.resolve()),
});

vi.mock("@shared/providers/workflows/analytics/page-view", () => ({
  PageViewWorkflowProvider: {
    record: (...args: unknown[]) => {
      mockPageViewRecord(...args);
      return createMockAsyncResult();
    },
  },
}));

vi.mock("@shared/providers/workflows/analytics/unique-visitor", () => ({
  UniqueVisitorWorkflowProvider: {
    record: (...args: unknown[]) => {
      mockUniqueVisitorRecord(...args);
      return createMockAsyncResult();
    },
  },
}));

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

function createHeadersGetMock(options: {
  referer?: string;
  userAgent?: string;
}) {
  return (name: string) => {
    if (name === "referer") {
      return options.referer ?? null;
    }
    if (name === "user-agent") {
      return options.userAgent ?? null;
    }
    return null;
  };
}

describe("view actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockCookies.get.mockReset();
    mockCookies.set.mockReset();
    mockHeaders.get.mockReset();
    mockPageViewRecord.mockReset();
    mockUniqueVisitorRecord.mockReset();
    pendingAfterCallbacks.length = 0;
  });

  describe("incrementViewCount", () => {
    it("初回訪問時にビューカウントをインクリメントする", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(1);
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      mockHeaders.get.mockImplementation(createHeadersGetMock({}));

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);
      await flushAfterCallbacks();

      expect(mockCookies.set).toHaveBeenCalled();
      expect(mockPageViewRecord).toHaveBeenCalledWith(
        expect.objectContaining({
          payload: expect.objectContaining({
            reference: identifier,
          }),
        }),
      );
      expect(mockUniqueVisitorRecord).toHaveBeenCalled();
    });

    it("既存のセッションがある場合はcookieを設定しない", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(2);
      mockCookies.get.mockImplementation(
        createCookieGetMock({ pageViewSession: "existing-session-id" }),
      );
      mockHeaders.get.mockImplementation(createHeadersGetMock({}));

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);
      await flushAfterCallbacks();

      expect(mockCookies.set).not.toHaveBeenCalled();
      expect(mockPageViewRecord).toHaveBeenCalledWith(
        expect.objectContaining({
          payload: expect.objectContaining({
            sessionKey: "existing-session-id",
          }),
        }),
      );
    });

    it("管理者セッションがある場合はカウントしない", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(3);
      mockCookies.get.mockImplementation(
        createCookieGetMock({ adminSession: "admin-session" }),
      );

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);

      expect(mockPageViewRecord).not.toHaveBeenCalled();
      expect(mockUniqueVisitorRecord).not.toHaveBeenCalled();
    });

    it("記事タイプのidentifierを正しく処理する", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(6, {
        type: ContentType.ARTICLE,
      });
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      mockHeaders.get.mockImplementation(createHeadersGetMock({}));

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);
      await flushAfterCallbacks();

      expect(mockPageViewRecord).toHaveBeenCalledWith(
        expect.objectContaining({
          payload: expect.objectContaining({
            reference: identifier,
          }),
        }),
      );
    });

    it("メモタイプのidentifierを正しく処理する", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(7, {
        type: ContentType.MEMO,
      });
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      mockHeaders.get.mockImplementation(createHeadersGetMock({}));

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);
      await flushAfterCallbacks();

      expect(mockPageViewRecord).toHaveBeenCalledWith(
        expect.objectContaining({
          payload: expect.objectContaining({
            reference: identifier,
          }),
        }),
      );
    });

    it("シリーズタイプのidentifierを正しく処理する", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(8, {
        type: ContentType.SERIES,
      });
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      mockHeaders.get.mockImplementation(createHeadersGetMock({}));

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);
      await flushAfterCallbacks();

      expect(mockPageViewRecord).toHaveBeenCalledWith(
        expect.objectContaining({
          payload: expect.objectContaining({
            reference: identifier,
          }),
        }),
      );
    });

    it("リファラーとUser-Agentがheadersから取得される", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(9);
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      mockHeaders.get.mockImplementation(
        createHeadersGetMock({
          referer: "https://google.com",
          userAgent: "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X)",
        }),
      );

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);
      await flushAfterCallbacks();

      expect(mockHeaders.get).toHaveBeenCalledWith("referer");
      expect(mockHeaders.get).toHaveBeenCalledWith("user-agent");
      expect(mockPageViewRecord).toHaveBeenCalledWith(
        expect.objectContaining({
          payload: expect.objectContaining({
            referrer: "https://google.com",
            userAgent: "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X)",
          }),
        }),
      );
    });

    it("PageView と UniqueVisitor のワークフローが並列で呼ばれる", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(10);
      mockCookies.get.mockImplementation(createCookieGetMock({}));
      mockHeaders.get.mockImplementation(createHeadersGetMock({}));

      const { incrementViewCount } = await import("@shared/actions/view");
      await incrementViewCount(identifier);
      await flushAfterCallbacks();

      expect(mockPageViewRecord).toHaveBeenCalledTimes(1);
      expect(mockUniqueVisitorRecord).toHaveBeenCalledTimes(1);
    });
  });
});
