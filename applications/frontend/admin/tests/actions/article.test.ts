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

const mockArticleWorkflowCreate = vi.fn();
const mockArticleWorkflowEdit = vi.fn();
const mockArticleWorkflowTerminate = vi.fn();
const mockArticleWorkflowFindBySlug = vi.fn();
const mockArticleWorkflowSearch = vi.fn();

vi.mock("@/providers/workflows/article", () => ({
  AdminArticleWorkflowProvider: {
    create: mockArticleWorkflowCreate,
    edit: mockArticleWorkflowEdit,
    terminate: mockArticleWorkflowTerminate,
    findBySlug: mockArticleWorkflowFindBySlug,
    search: mockArticleWorkflowSearch,
  },
}));

const unvalidated = {
  identifier: "01HWXYZ0000000000000000000",
  title: "テスト記事",
  content: "# テスト",
  excerpt: "テスト",
  slug: "test-article",
  status: "draft" as const,
  tags: [],
  images: [],
  timeline: { createdAt: new Date(), updatedAt: new Date() },
};

describe("actions/article", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
    mockRequireAdmin.mockResolvedValue(undefined);
    mockUnwrapForNextJs.mockImplementation((asyncResult: Promise<unknown>) => asyncResult);
    mockEventBrokerPublish.mockReturnValue(ok(undefined).toAsync());
  });

  describe("create", () => {
    beforeEach(() => {
      mockArticleWorkflowCreate.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("requireAdmin を呼び出す", async () => {
      const { create } = await import("@/actions/article");

      await create(unvalidated);

      expect(mockRequireAdmin).toHaveBeenCalled();
    });

    it("AdminArticleWorkflowProvider.create を呼び出す", async () => {
      const { create } = await import("@/actions/article");

      await create(unvalidated);

      expect(mockArticleWorkflowCreate).toHaveBeenCalledWith(
        expect.objectContaining({ payload: unvalidated }),
      );
    });

    it("revalidateTag を articles で呼び出す", async () => {
      const { create } = await import("@/actions/article");

      await create(unvalidated);

      expect(mockRevalidateTag).toHaveBeenCalledWith("articles", {});
    });

    it("notifyReaderRevalidation を ARTICLES タグで呼び出す", async () => {
      const { create } = await import("@/actions/article");

      await create(unvalidated);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(["articles"]);
    });
  });

  describe("edit", () => {
    const before = {
      identifier: "01HWXYZ0000000000000000000",
      title: "旧タイトル",
      slug: "test-article",
      status: "draft" as const,
    };

    beforeEach(() => {
      mockArticleWorkflowEdit.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("revalidateTag を articles で呼び出す", async () => {
      const { edit } = await import("@/actions/article");

      await edit(unvalidated, before);

      expect(mockRevalidateTag).toHaveBeenCalledWith("articles", {});
    });

    it("notifyReaderRevalidation を ARTICLES タグで呼び出す", async () => {
      const { edit } = await import("@/actions/article");

      await edit(unvalidated, before);

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(["articles"]);
    });
  });

  describe("terminate", () => {
    beforeEach(() => {
      mockArticleWorkflowTerminate.mockReturnValue(createMockAsyncResult(undefined));
    });

    it("revalidateTag を articles で呼び出す", async () => {
      const { terminate } = await import("@/actions/article");

      await terminate("01HWXYZ0000000000000000000");

      expect(mockRevalidateTag).toHaveBeenCalledWith("articles", {});
    });

    it("notifyReaderRevalidation を ARTICLES タグで呼び出す", async () => {
      const { terminate } = await import("@/actions/article");

      await terminate("01HWXYZ0000000000000000000");

      expect(mockNotifyReaderRevalidation).toHaveBeenCalledWith(["articles"]);
    });
  });
});
