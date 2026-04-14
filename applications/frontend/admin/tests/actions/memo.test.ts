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
  memoEntriesTag: (slug: string) => `memo-entries-${slug}`,
}));

const createMockAsyncResult = (value: unknown) => ({
  andThen: vi.fn().mockReturnValue(ok(value).toAsync()),
  map: vi.fn().mockReturnValue(ok(value).toAsync()),
});

const mockMemoWorkflowCreate = vi.fn();
const mockMemoWorkflowAddEntry = vi.fn();
const mockMemoWorkflowEdit = vi.fn();
const mockMemoWorkflowSearch = vi.fn();
const mockMemoWorkflowTerminate = vi.fn();
const mockMemoWorkflowFindBySlug = vi.fn();

vi.mock("@/providers/workflows/memo", () => ({
  AdminMemoWorkflowProvider: {
    create: mockMemoWorkflowCreate,
    addEntry: mockMemoWorkflowAddEntry,
    edit: mockMemoWorkflowEdit,
    search: mockMemoWorkflowSearch,
    terminate: mockMemoWorkflowTerminate,
    findBySlug: mockMemoWorkflowFindBySlug,
  },
}));

const unvalidated = {
  identifier: "01HWXYZ0000000000000000000",
  title: "テストメモ",
  slug: "test-memo",
  status: "draft" as const,
  tags: [],
  timeline: { createdAt: new Date(), updatedAt: new Date() },
};

describe("actions/memo", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockImplementation((asyncResult: Promise<unknown>) => asyncResult);
    mockEventBrokerPublish.mockReturnValue(ok(undefined).toAsync());
  });

  describe("create", () => {
    beforeEach(() => {
      mockMemoWorkflowCreate.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("revalidateTag を memos で呼び出す", async () => {
      const { create } = await import("@/actions/memo");

      await create(unvalidated);

      expect(mockRevalidateTag).toHaveBeenCalledWith("memos", {});
    });

    it("notifyReaderRevalidation を MEMOS タグで呼び出す", async () => {
      const { create } = await import("@/actions/memo");

      await create(unvalidated);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(["memos"]);
    });
  });

  describe("addEntry", () => {
    const slug = "test-memo";
    const unvalidatedEntry = {
      content: "エントリ内容",
      images: [],
      timeline: { createdAt: new Date(), updatedAt: new Date() },
    };

    beforeEach(() => {
      mockMemoWorkflowAddEntry.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("revalidateTag を memo-entries-{slug} で呼び出す", async () => {
      const { addEntry } = await import("@/actions/memo");

      await addEntry(unvalidatedEntry, slug, []);

      expect(mockRevalidateTag).toHaveBeenCalledWith(`memo-entries-${slug}`, {});
    });

    it("notifyReaderRevalidation を memoEntriesTag と MEMOS タグで呼び出す", async () => {
      const { addEntry } = await import("@/actions/memo");

      await addEntry(unvalidatedEntry, slug, []);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith([
        `memo-entries-${slug}`,
        "memos",
      ]);
    });
  });

  describe("edit", () => {
    const before = {
      identifier: "01HWXYZ0000000000000000000",
      title: "旧タイトル",
      slug: "test-memo",
      status: "draft" as const,
    };

    beforeEach(() => {
      mockMemoWorkflowEdit.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("revalidateTag を memos で呼び出す", async () => {
      const { edit } = await import("@/actions/memo");

      await edit(unvalidated, before);

      expect(mockRevalidateTag).toHaveBeenCalledWith("memos", {});
    });

    it("notifyReaderRevalidation を MEMOS タグで呼び出す", async () => {
      const { edit } = await import("@/actions/memo");

      await edit(unvalidated, before);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(["memos"]);
    });
  });

  describe("terminate", () => {
    beforeEach(() => {
      mockMemoWorkflowTerminate.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("revalidateTag を memos で呼び出す", async () => {
      const { terminate } = await import("@/actions/memo");

      await terminate("01HWXYZ0000000000000000000");

      expect(mockRevalidateTag).toHaveBeenCalledWith("memos", {});
    });

    it("notifyReaderRevalidation を MEMOS タグで呼び出す", async () => {
      const { terminate } = await import("@/actions/memo");

      await terminate("01HWXYZ0000000000000000000");

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(["memos"]);
    });
  });
});
