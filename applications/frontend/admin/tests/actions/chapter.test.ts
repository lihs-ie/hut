/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

const mockRequireAdmin = vi.fn();
const mockRevalidateTag = vi.fn();
const mockUnwrapForNextJs = vi.fn();
const mockChapterWorkflowPersist = vi.fn();
const mockChapterWorkflowFindBySlug = vi.fn();

vi.mock("@/aspects/auth-guard", () => ({
  requireAdmin: mockRequireAdmin,
}));

vi.mock("next/cache", () => ({
  revalidateTag: mockRevalidateTag,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: mockUnwrapForNextJs,
}));

const mockChapterWorkflowTerminate = vi.fn();

vi.mock("@/providers/workflows/chapter", () => ({
  AdminChapterWorkflowProvider: {
    persist: mockChapterWorkflowPersist,
    findBySlug: mockChapterWorkflowFindBySlug,
    terminate: mockChapterWorkflowTerminate,
  },
}));

const mockSeriesWorkflowFindBySlug = vi.fn();
const mockSeriesWorkflowPersist = vi.fn();

vi.mock("@/providers/workflows/series", () => ({
  AdminSeriesWorkflowProvider: {
    findBySlug: mockSeriesWorkflowFindBySlug,
    persist: mockSeriesWorkflowPersist,
  },
}));

vi.mock("@shared/domains/series", () => ({
  addChapter: vi.fn(),
  removeChapter: vi.fn(),
}));

describe("actions/chapter", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockImplementation((asyncResult: Promise<unknown>) => asyncResult);
  });

  describe("persist", () => {
    it("requireAdminを呼び出す", async () => {
      const { persist } = await import("@/actions/chapter");

      await persist({
        identifier: "01HWXYZ0000000000000000000",
        title: "テストチャプター",
        slug: "test-chapter",
        content: "# テスト",
        images: [],
        status: "DRAFT",
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      });

      expect(mockRequireAdmin).toHaveBeenCalled();
    });

    it("AdminChapterWorkflowProvider.persistを呼び出す", async () => {
      const { persist } = await import("@/actions/chapter");

      const unvalidated = {
        identifier: "01HWXYZ0000000000000000000",
        title: "テストチャプター",
        slug: "test-chapter",
        content: "# テスト",
        images: [],
        status: "DRAFT",
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      };

      await persist(unvalidated);

      expect(mockChapterWorkflowPersist).toHaveBeenCalledWith(unvalidated);
    });

    it("revalidateTagを chapters で呼び出す", async () => {
      const { persist } = await import("@/actions/chapter");

      await persist({
        identifier: "01HWXYZ0000000000000000000",
        title: "テストチャプター",
        slug: "test-chapter",
        content: "# テスト",
        images: [],
        status: "DRAFT",
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      });

      expect(mockRevalidateTag).toHaveBeenCalledWith("chapters", {});
      expect(mockRevalidateTag).toHaveBeenCalledWith("series", {});
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

  describe("terminate", () => {
    const defaultSeries = {
      identifier: "01HWXYZ0000000000000000001",
      title: "テスト連載",
      slug: "test-series",
      chapters: ["01HWXYZ0000000000000000000"],
      tags: [],
      subTitle: null,
      status: "DRAFT",
      timeline: { createdAt: new Date(), updatedAt: new Date() },
    };

    it("requireAdminを呼び出す", async () => {
      const { terminate } = await import("@/actions/chapter");
      const { removeChapter } = await import("@shared/domains/series");
      vi.mocked(removeChapter).mockReturnValue({ ...defaultSeries, chapters: [] } as never);

      mockSeriesWorkflowFindBySlug.mockReturnValue(Promise.resolve(defaultSeries));
      mockSeriesWorkflowPersist.mockReturnValue(Promise.resolve(undefined));

      await terminate("01HWXYZ0000000000000000000", "test-series");

      expect(mockRequireAdmin).toHaveBeenCalled();
    });

    it("AdminChapterWorkflowProvider.terminateをchapterIdentifierで呼び出す", async () => {
      const { terminate } = await import("@/actions/chapter");
      const { removeChapter } = await import("@shared/domains/series");
      const chapterIdentifier = "01HWXYZ0000000000000000000";
      vi.mocked(removeChapter).mockReturnValue({ ...defaultSeries, chapters: [] } as never);

      mockSeriesWorkflowFindBySlug.mockReturnValue(Promise.resolve(defaultSeries));
      mockSeriesWorkflowPersist.mockReturnValue(Promise.resolve(undefined));

      await terminate(chapterIdentifier, "test-series");

      expect(mockChapterWorkflowTerminate).toHaveBeenCalledWith(chapterIdentifier);
    });

    it("Seriesからchapterを除去してpersistする", async () => {
      const { terminate } = await import("@/actions/chapter");
      const chapterIdentifier = "01HWXYZ0000000000000000000";
      const seriesSlug = "test-series";
      const series = {
        identifier: "01HWXYZ0000000000000000001",
        title: "テスト連載",
        slug: seriesSlug,
        chapters: [chapterIdentifier],
        tags: [],
        subTitle: null,
        status: "DRAFT",
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      };

      mockSeriesWorkflowFindBySlug.mockReturnValue(Promise.resolve(series));
      mockSeriesWorkflowPersist.mockReturnValue(Promise.resolve(undefined));

      const { removeChapter } = await import("@shared/domains/series");
      const updatedSeries = { ...series, chapters: [] };
      vi.mocked(removeChapter).mockReturnValue(updatedSeries as never);

      await terminate(chapterIdentifier, seriesSlug);

      expect(mockSeriesWorkflowFindBySlug).toHaveBeenCalledWith({
        payload: { slug: seriesSlug },
        now: expect.any(Date),
      });
      expect(removeChapter).toHaveBeenCalledWith(series, chapterIdentifier);
      expect(mockSeriesWorkflowPersist).toHaveBeenCalled();
    });

    it("revalidateTagを chapters と series で呼び出す", async () => {
      const { terminate } = await import("@/actions/chapter");

      mockSeriesWorkflowFindBySlug.mockReturnValue(Promise.resolve({
        identifier: "01HWXYZ0000000000000000001",
        title: "テスト連載",
        slug: "test-series",
        chapters: ["01HWXYZ0000000000000000000"],
        tags: [],
        subTitle: null,
        status: "DRAFT",
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      }));
      mockSeriesWorkflowPersist.mockReturnValue(Promise.resolve(undefined));

      await terminate("01HWXYZ0000000000000000000", "test-series");

      expect(mockRevalidateTag).toHaveBeenCalledWith("chapters", {});
      expect(mockRevalidateTag).toHaveBeenCalledWith("series", {});
    });
  });
});
