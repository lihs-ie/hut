/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { ok } from "@shared/aspects/result";

const mockRequireAdmin = vi.fn();
const mockRevalidateTag = vi.fn();
const mockUnwrapForNextJs = vi.fn();
const mockEventBrokerPublish = vi.fn();
const mockNotifyReaderRevalidation = vi.fn();

vi.mock("@/aspects/auth-guard", () => ({
  requireAdmin: mockRequireAdmin,
}));

vi.mock("next/cache", () => ({
  revalidateTag: mockRevalidateTag,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: mockUnwrapForNextJs,
}));

vi.mock("@/providers/domain/event", () => ({
  EventBrokerProvider: {
    pubSub: {
      publish: mockEventBrokerPublish,
    },
  },
}));

vi.mock("@/aspects/revalidation", () => ({
  notifyReaderRevalidation: mockNotifyReaderRevalidation,
}));

vi.mock("@shared/config/revalidation", () => ({
  REVALIDATION_TAGS: {
    ARTICLES: "articles",
    MEMOS: "memos",
    SERIES: "series",
    CHAPTERS: "chapters",
    TAGS: "tags",
    PRIVACY_POLICY: "privacy-policy",
  },
}));

const createMockAsyncResult = (value: unknown) => ({
  andThen: vi.fn().mockReturnValue(ok(value).toAsync()),
});

const mockSeriesWorkflowPersist = vi.fn();
const mockSeriesWorkflowTerminateWithChapters = vi.fn();
const mockSeriesWorkflowFindBySlug = vi.fn();
const mockSeriesWorkflowSearch = vi.fn();

vi.mock("@/providers/workflows/series", () => ({
  AdminSeriesWorkflowProvider: {
    persist: mockSeriesWorkflowPersist,
    terminateWithChapters: mockSeriesWorkflowTerminateWithChapters,
    findBySlug: mockSeriesWorkflowFindBySlug,
    search: mockSeriesWorkflowSearch,
  },
}));

const unvalidated = {
  identifier: "01HWXYZ0000000000000000000",
  title: "テストシリーズ",
  slug: "test-series",
  description: "テスト説明",
  status: "draft" as const,
  chapters: [],
  timeline: { createdAt: new Date(), updatedAt: new Date() },
};

describe("actions/series", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockImplementation((asyncResult: Promise<unknown>) => asyncResult);
    mockEventBrokerPublish.mockReturnValue(ok(undefined).toAsync());
  });

  describe("persist", () => {
    beforeEach(() => {
      mockSeriesWorkflowPersist.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("revalidateTag を series で呼び出す", async () => {
      const { persist } = await import("@/actions/series");

      await persist(unvalidated);

      expect(mockRevalidateTag).toHaveBeenCalledWith("series", {});
    });

    it("notifyReaderRevalidation を SERIES タグで呼び出す", async () => {
      const { persist } = await import("@/actions/series");

      await persist(unvalidated);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(["series"]);
    });
  });

  describe("terminate", () => {
    beforeEach(() => {
      mockSeriesWorkflowTerminateWithChapters.mockReturnValue(
        createMockAsyncResult(undefined),
      );
    });

    it("revalidateTag を series で呼び出す", async () => {
      const { terminate } = await import("@/actions/series");

      await terminate("01HWXYZ0000000000000000000");

      expect(mockRevalidateTag).toHaveBeenCalledWith("series", {});
    });

    it("notifyReaderRevalidation を SERIES タグで呼び出す", async () => {
      const { terminate } = await import("@/actions/series");

      await terminate("01HWXYZ0000000000000000000");

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(["series"]);
    });
  });
});
