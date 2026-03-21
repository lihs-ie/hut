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

vi.mock("@/providers/workflows/chapter", () => ({
  AdminChapterWorkflowProvider: {
    persist: mockChapterWorkflowPersist,
    findBySlug: mockChapterWorkflowFindBySlug,
  },
}));

describe("actions/chapter", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockResolvedValue(undefined);
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
});
