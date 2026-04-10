/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";

const mockRequireAdmin = vi.fn();
const mockRevalidateTag = vi.fn();
const mockUnwrapForNextJs = vi.fn();
const mockNotifyReaderRevalidation = vi.fn();
const mockTagWorkflowPersist = vi.fn();
const mockTagWorkflowTerminate = vi.fn();

vi.mock("@/aspects/auth-guard", () => ({
  requireAdmin: mockRequireAdmin,
}));

vi.mock("next/cache", () => ({
  revalidateTag: mockRevalidateTag,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: mockUnwrapForNextJs,
}));

vi.mock("@/aspects/revalidation", () => ({
  notifyReaderRevalidation: mockNotifyReaderRevalidation,
}));

vi.mock("@shared/aspects/result", async (importOriginal) => {
  const original = await importOriginal<typeof import("@shared/aspects/result")>();
  return original;
});

vi.mock("@/providers/workflows/tag", () => ({
  AdminTagWorkflowProvider: {
    persist: mockTagWorkflowPersist,
    terminate: mockTagWorkflowTerminate,
    ofIdentifiers: vi.fn(),
    ofNames: vi.fn(),
    search: vi.fn(),
  },
}));

describe("actions/tag", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockResolvedValue(undefined);
    mockNotifyReaderRevalidation.mockReturnValue(undefined);
  });

  describe("persist", () => {
    const unvalidated = {
      name: "テストタグ",
    };

    beforeEach(() => {
      mockTagWorkflowPersist.mockReturnValue({ toAsync: vi.fn() });
    });

    it("Admin のキャッシュを tags タグで revalidateTag する", async () => {
      const { persist } = await import("@/actions/tag");

      await persist(unvalidated);

      expect(mockRevalidateTag).toHaveBeenCalledWith("tags", {});
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { persist } = await import("@/actions/tag");

      await persist(unvalidated);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["tags"]),
      );
    });
  });

  describe("terminate", () => {
    const identifier = "01HWXYZ0000000000000000006";

    beforeEach(() => {
      mockTagWorkflowTerminate.mockReturnValue({ toAsync: vi.fn() });
    });

    it("Admin のキャッシュを tags タグで revalidateTag する", async () => {
      const { terminate } = await import("@/actions/tag");

      await terminate(identifier);

      expect(mockRevalidateTag).toHaveBeenCalledWith("tags", {});
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { terminate } = await import("@/actions/tag");

      await terminate(identifier);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["tags"]),
      );
    });
  });
});
