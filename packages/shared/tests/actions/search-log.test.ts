/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

const mockSearchRecordRecord = vi.fn();

const createMockAsyncResult = () => ({
  tapError: vi.fn().mockReturnValue(Promise.resolve()),
});

vi.mock("@shared/providers/workflows/analytics/search-record", () => ({
  SearchRecordWorkflowProvider: {
    record: (...args: unknown[]) => {
      mockSearchRecordRecord(...args);
      return createMockAsyncResult();
    },
  },
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

describe("search-log actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockSearchRecordRecord.mockReset();
    pendingAfterCallbacks.length = 0;
  });

  describe("recordSearchLog", () => {
    it("freeWordが存在する場合にワークフローを呼び出す", async () => {
      const { recordSearchLog } = await import("@shared/actions/search-log");

      await recordSearchLog(
        {
          freeWord: "React",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
        5,
      );
      await flushAfterCallbacks();

      expect(mockSearchRecordRecord).toHaveBeenCalledWith(
        expect.objectContaining({
          payload: expect.objectContaining({
            keyword: "React",
            resultCount: 5,
            tags: null,
            contentType: null,
          }),
        }),
      );
    });

    it("freeWordがnullの場合はワークフローを呼び出さない", async () => {
      const { recordSearchLog } = await import("@shared/actions/search-log");

      await recordSearchLog(
        {
          freeWord: null,
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
        5,
      );

      expect(mockSearchRecordRecord).not.toHaveBeenCalled();
    });

    it("freeWordが空文字の場合はワークフローを呼び出さない", async () => {
      const { recordSearchLog } = await import("@shared/actions/search-log");

      await recordSearchLog(
        {
          freeWord: "",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
        5,
      );

      expect(mockSearchRecordRecord).not.toHaveBeenCalled();
    });

    it("freeWordが空白のみの場合はワークフローを呼び出さない", async () => {
      const { recordSearchLog } = await import("@shared/actions/search-log");

      await recordSearchLog(
        {
          freeWord: "   ",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
        5,
      );

      expect(mockSearchRecordRecord).not.toHaveBeenCalled();
    });

    it("検索条件からtags, typeが正しくワークフローに渡される", async () => {
      const { recordSearchLog } = await import("@shared/actions/search-log");

      await recordSearchLog(
        {
          freeWord: "Next.js",
          tags: ["nextjs", "react"],
          type: "article",
          sortBy: "latest",
          order: "desc",
          limit: 10,
        },
        3,
      );
      await flushAfterCallbacks();

      expect(mockSearchRecordRecord).toHaveBeenCalledWith(
        expect.objectContaining({
          payload: expect.objectContaining({
            keyword: "Next.js",
            resultCount: 3,
            tags: ["nextjs", "react"],
            contentType: "article",
          }),
        }),
      );
    });

    it("freeWordの前後の空白がトリムされる", async () => {
      const { recordSearchLog } = await import("@shared/actions/search-log");

      await recordSearchLog(
        {
          freeWord: "  React  ",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
        2,
      );
      await flushAfterCallbacks();

      expect(mockSearchRecordRecord).toHaveBeenCalledWith(
        expect.objectContaining({
          payload: expect.objectContaining({
            keyword: "React",
          }),
        }),
      );
    });

    it("now が Date インスタンスとしてワークフローに渡される", async () => {
      const { recordSearchLog } = await import("@shared/actions/search-log");

      await recordSearchLog(
        {
          freeWord: "test",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
        1,
      );
      await flushAfterCallbacks();

      expect(mockSearchRecordRecord).toHaveBeenCalledWith(
        expect.objectContaining({
          now: expect.any(Date),
        }),
      );
    });
  });
});
