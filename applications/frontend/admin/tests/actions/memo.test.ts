/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { ok } from "@shared/aspects/result";

const mockRequireAdmin = vi.fn();
const mockRevalidateTag = vi.fn();
const mockUnwrapForNextJs = vi.fn();
const mockNotifyReaderRevalidation = vi.fn();
const mockEventBrokerPublish = vi.fn();
const mockMemoWorkflowCreate = vi.fn();
const mockMemoWorkflowAddEntry = vi.fn();
const mockMemoWorkflowEdit = vi.fn();
const mockMemoWorkflowTerminate = vi.fn();

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

vi.mock("@/providers/workflows/memo", () => ({
  AdminMemoWorkflowProvider: {
    create: mockMemoWorkflowCreate,
    addEntry: mockMemoWorkflowAddEntry,
    edit: mockMemoWorkflowEdit,
    terminate: mockMemoWorkflowTerminate,
    findBySlug: vi.fn(),
    search: vi.fn(),
  },
}));

const createMockAsyncResult = (value: unknown) => ({
  andThen: vi.fn().mockReturnValue(ok(value).toAsync()),
  map: vi.fn().mockReturnValue(ok(value).toAsync()),
});

describe("actions/memo", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockImplementation(
      (asyncResult: Promise<unknown>) => asyncResult,
    );
    mockEventBrokerPublish.mockReturnValue(ok(undefined).toAsync());
    mockNotifyReaderRevalidation.mockResolvedValue(undefined);
  });

  describe("create", () => {
    const unvalidated = {
      title: "テストメモ",
      slug: "test-memo",
      status: "DRAFT",
      tags: [],
      images: [],
    };

    beforeEach(() => {
      mockMemoWorkflowCreate.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("Admin のキャッシュを revalidateTag で無効化する", async () => {
      const { create } = await import("@/actions/memo");

      await create(unvalidated);

      expect(mockRevalidateTag).toHaveBeenCalledWith("memos", {});
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { create } = await import("@/actions/memo");

      await create(unvalidated);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["memos"]),
      );
    });
  });

  describe("addEntry", () => {
    const unvalidated = {
      text: "テストエントリー",
    };
    const slug = "test-memo";
    const images = [];

    beforeEach(() => {
      mockMemoWorkflowAddEntry.mockReturnValue(
        createMockAsyncResult(undefined),
      );
    });

    it("Admin のキャッシュを memo-entries タグで revalidateTag する", async () => {
      const { addEntry } = await import("@/actions/memo");

      await addEntry(unvalidated, slug, images);

      expect(mockRevalidateTag).toHaveBeenCalledWith(
        `memo-entries-${slug}`,
        { expire: 3600 },
      );
    });

    it("Reader の revalidation を memo-entries タグで通知する", async () => {
      const { addEntry } = await import("@/actions/memo");

      await addEntry(unvalidated, slug, images);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining([`memo-entries-${slug}`]),
      );
    });
  });

  describe("edit", () => {
    const unvalidated = {
      title: "編集メモ",
      slug: "edited-memo",
      status: "PUBLISHED",
      tags: [],
      images: [],
    };
    const before = {
      identifier: "01HWXYZ0000000000000000003",
      slug: "original-memo",
    };

    beforeEach(() => {
      mockMemoWorkflowEdit.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("Admin のキャッシュを revalidateTag で無効化する", async () => {
      const { edit } = await import("@/actions/memo");

      await edit(unvalidated, before);

      expect(mockRevalidateTag).toHaveBeenCalledWith("memos", {});
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { edit } = await import("@/actions/memo");

      await edit(unvalidated, before);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["memos"]),
      );
    });
  });

  describe("terminate", () => {
    const identifier = "01HWXYZ0000000000000000004";

    beforeEach(() => {
      mockMemoWorkflowTerminate.mockReturnValue(
        createMockAsyncResult(undefined),
      );
    });

    it("Admin のキャッシュを revalidateTag で無効化する", async () => {
      const { terminate } = await import("@/actions/memo");

      await terminate(identifier);

      expect(mockRevalidateTag).toHaveBeenCalledWith("memos", {});
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { terminate } = await import("@/actions/memo");

      await terminate(identifier);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["memos"]),
      );
    });
  });
});
