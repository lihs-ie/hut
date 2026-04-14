/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { ok } from "@shared/aspects/result";

const mockRequireAdmin = vi.fn();
const mockRevalidateTag = vi.fn();
const mockUnwrapForNextJs = vi.fn();
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

vi.mock("@shared/aspects/error", () => ({
  aggregateNotFoundError: vi.fn(),
}));

vi.mock("@shared/aspects/result", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@shared/aspects/result")>();
  return actual;
});

vi.mock("react", () => ({
  cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

const mockTagWorkflowPersist = vi.fn();
const mockTagWorkflowTerminate = vi.fn();
const mockTagWorkflowOfIdentifiers = vi.fn();
const mockTagWorkflowSearch = vi.fn();
const mockTagWorkflowOfNames = vi.fn();

vi.mock("@/providers/workflows/tag", () => ({
  AdminTagWorkflowProvider: {
    persist: mockTagWorkflowPersist,
    terminate: mockTagWorkflowTerminate,
    ofIdentifiers: mockTagWorkflowOfIdentifiers,
    search: mockTagWorkflowSearch,
    ofNames: mockTagWorkflowOfNames,
  },
}));

const unvalidated = {
  identifier: "01HWXYZ0000000000000000000",
  name: "テストタグ",
  timeline: { createdAt: new Date(), updatedAt: new Date() },
};

describe("actions/tag", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockImplementation((asyncResult: Promise<unknown>) => asyncResult);
  });

  describe("persist", () => {
    beforeEach(() => {
      mockTagWorkflowPersist.mockReturnValue(ok(undefined).toAsync());
    });

    it("revalidateTag を tags で呼び出す", async () => {
      const { persist } = await import("@/actions/tag");

      await persist(unvalidated);

      expect(mockRevalidateTag).toHaveBeenCalledWith("tags", {});
    });

    it("notifyReaderRevalidation を TAGS タグで呼び出す", async () => {
      const { persist } = await import("@/actions/tag");

      await persist(unvalidated);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(["tags"]);
    });
  });

  describe("terminate", () => {
    beforeEach(() => {
      mockTagWorkflowTerminate.mockReturnValue(ok(undefined).toAsync());
    });

    it("revalidateTag を tags で呼び出す", async () => {
      const { terminate } = await import("@/actions/tag");

      await terminate("01HWXYZ0000000000000000000");

      expect(mockRevalidateTag).toHaveBeenCalledWith("tags", {});
    });

    it("notifyReaderRevalidation を TAGS タグで呼び出す", async () => {
      const { terminate } = await import("@/actions/tag");

      await terminate("01HWXYZ0000000000000000000");

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(["tags"]);
    });
  });
});
