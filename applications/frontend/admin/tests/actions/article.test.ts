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
const mockArticleWorkflowCreate = vi.fn();
const mockArticleWorkflowEdit = vi.fn();
const mockArticleWorkflowTerminate = vi.fn();

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

vi.mock("@/providers/workflows/article", () => ({
  AdminArticleWorkflowProvider: {
    create: mockArticleWorkflowCreate,
    edit: mockArticleWorkflowEdit,
    terminate: mockArticleWorkflowTerminate,
    findBySlug: vi.fn(),
    search: vi.fn(),
  },
}));

const createMockAsyncResult = (value: unknown) => ({
  andThen: vi.fn().mockReturnValue(ok(value).toAsync()),
});

describe("actions/article", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockImplementation(
      (asyncResult: Promise<unknown>) => asyncResult,
    );
    mockEventBrokerPublish.mockReturnValue(ok(undefined).toAsync());
    mockNotifyReaderRevalidation.mockReturnValue(undefined);
  });

  describe("create", () => {
    const unvalidated = {
      title: "テスト記事",
      slug: "test-article",
      content: "# テスト",
      excerpt: "テスト抜粋",
      tags: [],
      images: [],
      status: "DRAFT",
    };

    beforeEach(() => {
      mockArticleWorkflowCreate.mockReturnValue(
        createMockAsyncResult(undefined),
      );
    });

    it("Admin のキャッシュを revalidateTag で無効化する", async () => {
      const { create } = await import("@/actions/article");

      await create(unvalidated);

      expect(mockRevalidateTag).toHaveBeenCalledWith("articles");
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { create } = await import("@/actions/article");

      await create(unvalidated);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["articles"]),
      );
    });
  });

  describe("edit", () => {
    const unvalidated = {
      title: "編集記事",
      slug: "edited-article",
      content: "# 編集",
      excerpt: "編集抜粋",
      tags: [],
      images: [],
      status: "PUBLISHED",
    };
    const before = {
      identifier: "01HWXYZ0000000000000000001",
      slug: "original-article",
    };

    beforeEach(() => {
      mockArticleWorkflowEdit.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("Admin のキャッシュを revalidateTag で無効化する", async () => {
      const { edit } = await import("@/actions/article");

      await edit(unvalidated, before);

      expect(mockRevalidateTag).toHaveBeenCalledWith("articles");
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { edit } = await import("@/actions/article");

      await edit(unvalidated, before);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["articles"]),
      );
    });
  });

  describe("terminate", () => {
    const identifier = "01HWXYZ0000000000000000002";

    beforeEach(() => {
      mockArticleWorkflowTerminate.mockReturnValue(
        createMockAsyncResult(undefined),
      );
    });

    it("Admin のキャッシュを revalidateTag で無効化する", async () => {
      const { terminate } = await import("@/actions/article");

      await terminate(identifier);

      expect(mockRevalidateTag).toHaveBeenCalledWith("articles");
    });

    it("Reader の revalidation を notifyReaderRevalidation で通知する", async () => {
      const { terminate } = await import("@/actions/article");

      await terminate(identifier);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(
        expect.arrayContaining(["articles"]),
      );
    });
  });
});
