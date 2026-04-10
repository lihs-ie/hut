/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { ok } from "@shared/aspects/result";

const mockRequireAdmin = vi.fn();
const mockRevalidateTag = vi.fn();
const mockUnwrapForNextJs = vi.fn();
const mockChapterWorkflowFindBySlug = vi.fn();
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

vi.mock("@/lib/revalidation", () => ({
  notifyReaderRevalidation: mockNotifyReaderRevalidation,
}));

const mockChapterWorkflowTerminate = vi.fn();
const mockChapterWorkflowPersistWithSeries = vi.fn();
const mockChapterWorkflowTerminateWithSeries = vi.fn();

const createMockAsyncResult = (value: unknown) => ({
  andThen: vi.fn().mockReturnValue(ok(value).toAsync()),
});

const mockChapterWorkflowPersist = vi.fn();

vi.mock("@shared/providers/infrastructure/chapter", () => ({
  ChapterRepositoryProvider: {
    firebase: {
      ofIdentifiers: vi.fn(),
    },
  },
}));

vi.mock("@/providers/workflows/chapter", () => ({
  AdminChapterWorkflowProvider: {
    persist: mockChapterWorkflowPersist,
    findBySlug: mockChapterWorkflowFindBySlug,
    terminate: mockChapterWorkflowTerminate,
    persistWithSeries: mockChapterWorkflowPersistWithSeries,
    terminateWithSeries: mockChapterWorkflowTerminateWithSeries,
  },
}));

describe("actions/chapter", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockImplementation((asyncResult: Promise<unknown>) => asyncResult);
    mockEventBrokerPublish.mockReturnValue(ok(undefined).toAsync());
    mockNotifyReaderRevalidation.mockResolvedValue(undefined);
  });

  describe("persist", () => {
    const unvalidated = {
      identifier: "01HWXYZ0000000000000000000",
      title: "テストチャプター",
      slug: "test-chapter",
      content: "# テスト",
      images: [],
      status: "DRAFT",
      timeline: { createdAt: new Date(), updatedAt: new Date() },
    };

    beforeEach(() => {
      mockChapterWorkflowPersist.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("requireAdminを呼び出す", async () => {
      const { persist } = await import("@/actions/chapter");

      await persist(unvalidated);

      expect(mockRequireAdmin).toHaveBeenCalled();
    });

    it("AdminChapterWorkflowProvider.persistを呼び出す", async () => {
      const { persist } = await import("@/actions/chapter");

      await persist(unvalidated);

      expect(mockChapterWorkflowPersist).toHaveBeenCalledWith(unvalidated);
    });

    it("revalidateTagを chapters で呼び出す", async () => {
      const { persist } = await import("@/actions/chapter");

      await persist(unvalidated);

      expect(mockRevalidateTag).toHaveBeenCalledWith("chapters", {});
      expect(mockRevalidateTag).toHaveBeenCalledWith("series", {});
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { persist } = await import("@/actions/chapter");

      await persist(unvalidated);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["chapters", "series"]),
      );
    });
  });

  describe("findBySlug", () => {
    it("requireAdminを呼び出す", async () => {
      const { findBySlug } = await import("@/actions/chapter");

      await findBySlug("test-chapter");

      expect(mockRequireAdmin).toHaveBeenCalled();
    });

    it("AdminChapterWorkflowProvider.findBySlugを正しいコマンドで呼び出す", async () => {
      const { findBySlug } = await import("@/actions/chapter");

      await findBySlug("test-chapter");

      expect(mockChapterWorkflowFindBySlug).toHaveBeenCalledWith({
        payload: { slug: "test-chapter" },
        now: expect.any(Date),
      });
    });
  });

  describe("findChapterBySlug", () => {
    it("findBySlugと同じ関数である", async () => {
      const { findBySlug, findChapterBySlug } = await import("@/actions/chapter");

      expect(findChapterBySlug).toBe(findBySlug);
    });
  });

  describe("findChaptersByIdentifiers", () => {
    it("requireAdminを呼び出す", async () => {
      const { findChaptersByIdentifiers } = await import("@/actions/chapter");

      await findChaptersByIdentifiers([]);

      expect(mockRequireAdmin).toHaveBeenCalled();
    });
  });

  describe("terminate", () => {
    it("requireAdminを呼び出す", async () => {
      const { terminate } = await import("@/actions/chapter");

      await terminate("01HWXYZ0000000000000000000", "test-series");

      expect(mockRequireAdmin).toHaveBeenCalled();
    });

    it("AdminChapterWorkflowProvider.terminateWithSeriesを呼び出す", async () => {
      const { terminate } = await import("@/actions/chapter");
      const chapterIdentifier = "01HWXYZ0000000000000000000";
      const seriesSlug = "test-series";

      await terminate(chapterIdentifier, seriesSlug);

      expect(mockChapterWorkflowTerminateWithSeries).toHaveBeenCalledWith(
        chapterIdentifier,
        seriesSlug,
      );
    });

    it("revalidateTagを chapters と series で呼び出す", async () => {
      const { terminate } = await import("@/actions/chapter");

      await terminate("01HWXYZ0000000000000000000", "test-series");

      expect(mockRevalidateTag).toHaveBeenCalledWith("chapters", {});
      expect(mockRevalidateTag).toHaveBeenCalledWith("series", {});
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { terminate } = await import("@/actions/chapter");

      await terminate("01HWXYZ0000000000000000000", "test-series");

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["chapters", "series"]),
      );
    });
  });
});
