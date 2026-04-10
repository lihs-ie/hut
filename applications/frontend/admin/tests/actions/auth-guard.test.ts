/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { ok } from "@shared/aspects/result";

vi.mock("@/actions/auth", () => ({
  isAdmin: vi.fn(),
}));

vi.mock("@/providers/workflows/article", () => ({
  AdminArticleWorkflowProvider: {
    create: vi.fn(),
    edit: vi.fn(),
    terminate: vi.fn(),
    search: vi.fn(),
  },
}));

vi.mock("@/providers/workflows/memo", () => ({
  AdminMemoWorkflowProvider: {
    create: vi.fn(),
    addEntry: vi.fn(),
    edit: vi.fn(),
    search: vi.fn(),
    terminate: vi.fn(),
  },
}));

vi.mock("@/providers/workflows/series", () => ({
  AdminSeriesWorkflowProvider: {
    persist: vi.fn(),
    terminate: vi.fn(),
  },
}));

vi.mock("@/providers/workflows/document", () => ({
  AdminDocumentWorkflowProvider: {
    PersistPrivatePolicy: vi.fn(),
  },
}));

vi.mock("@/providers/domain/event", () => ({
  EventBrokerProvider: {
    pubSub: {
      publish: vi.fn(),
    },
  },
}));

vi.mock("next/cache", () => ({
  revalidateTag: vi.fn(),
  revalidatePath: vi.fn(),
}));

vi.mock("@/config/revalidation", () => ({
  revalidation: {
    readerEndpoint: "https://reader.example.com",
    secret: "test-secret",
  },
}));

vi.mock("@/providers/infrastructure/storage", () => ({
  AdminImageUploaderProvider: {
    firebaseAdmin: {
      upload: vi.fn(),
    },
  },
}));

describe("Admin Server Actions - 認証チェック", () => {
  beforeEach(async () => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  describe("article actions", () => {
    it("未認証の場合 create は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminArticleWorkflowProvider } = await import(
        "@/providers/workflows/article"
      );
      vi.mocked(AdminArticleWorkflowProvider.create).mockReturnValue({
        andThen: vi.fn().mockReturnValue(ok(undefined).toAsync()),
      } as never);

      const { create } = await import("@/actions/article");

      await expect(
        create({
          identifier: "01JTEST000000000000000",
          title: "Test",
          content: "test",
          excerpt: "test",
          slug: "test",
          status: "draft",
          tags: [],
          images: [],
          timeline: { createdAt: new Date(), updatedAt: new Date() },
        }),
      ).rejects.toThrow("認証が必要です");

      expect(AdminArticleWorkflowProvider.create).not.toHaveBeenCalled();
    });

    it("未認証の場合 edit は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminArticleWorkflowProvider } = await import(
        "@/providers/workflows/article"
      );
      vi.mocked(AdminArticleWorkflowProvider.edit).mockReturnValue({
        andThen: vi.fn().mockReturnValue(ok(undefined).toAsync()),
      } as never);

      const { edit } = await import("@/actions/article");

      await expect(
        edit(
          {
            identifier: "01JTEST000000000000000",
            title: "Test",
            content: "test",
            excerpt: "test",
            slug: "test",
            status: "draft",
            tags: [],
            images: [],
            timeline: { createdAt: new Date(), updatedAt: new Date() },
          },
          {
            identifier: "01JTEST000000000000000",
            title: "Test",
            slug: "test",
            status: "draft",
          },
        ),
      ).rejects.toThrow("認証が必要です");

      expect(AdminArticleWorkflowProvider.edit).not.toHaveBeenCalled();
    });

    it("未認証の場合 terminate は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminArticleWorkflowProvider } = await import(
        "@/providers/workflows/article"
      );
      vi.mocked(AdminArticleWorkflowProvider.terminate).mockReturnValue({
        andThen: vi.fn().mockReturnValue(ok(undefined).toAsync()),
      } as never);

      const { terminate } = await import("@/actions/article");

      await expect(terminate("01JTEST000000000000000")).rejects.toThrow(
        "認証が必要です",
      );

      expect(AdminArticleWorkflowProvider.terminate).not.toHaveBeenCalled();
    });

    it("未認証の場合 search は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminArticleWorkflowProvider } = await import(
        "@/providers/workflows/article"
      );
      vi.mocked(AdminArticleWorkflowProvider.search).mockReturnValue(
        ok([]).toAsync(),
      );

      const { search } = await import("@/actions/article");

      await expect(
        search({
          keyword: "",
          status: "draft",
          tags: [],
        }),
      ).rejects.toThrow("認証が必要です");

      expect(AdminArticleWorkflowProvider.search).not.toHaveBeenCalled();
    });

    it("認証済みの場合 create はワークフローを呼び出す", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(true);

      const { AdminArticleWorkflowProvider } = await import(
        "@/providers/workflows/article"
      );
      vi.mocked(AdminArticleWorkflowProvider.create).mockReturnValue({
        andThen: vi.fn().mockReturnValue(ok(undefined).toAsync()),
      } as never);

      const { create } = await import("@/actions/article");

      await create({
        identifier: "01JTEST000000000000000",
        title: "Test",
        content: "test",
        excerpt: "test",
        slug: "test",
        status: "draft",
        tags: [],
        images: [],
        timeline: { createdAt: new Date(), updatedAt: new Date() },
      });

      expect(AdminArticleWorkflowProvider.create).toHaveBeenCalled();
    });
  });

  describe("memo actions", () => {
    it("未認証の場合 create は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminMemoWorkflowProvider } = await import(
        "@/providers/workflows/memo"
      );
      vi.mocked(AdminMemoWorkflowProvider.create).mockReturnValue({
        andThen: vi.fn().mockReturnValue(ok(undefined).toAsync()),
      } as never);

      const { create } = await import("@/actions/memo");

      await expect(
        create({
          identifier: "01JTEST000000000000000",
          title: "Test",
          slug: "test",
          status: "draft",
          tags: [],
          timeline: { createdAt: new Date(), updatedAt: new Date() },
        }),
      ).rejects.toThrow("認証が必要です");

      expect(AdminMemoWorkflowProvider.create).not.toHaveBeenCalled();
    });

    it("未認証の場合 terminate は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminMemoWorkflowProvider } = await import(
        "@/providers/workflows/memo"
      );
      vi.mocked(AdminMemoWorkflowProvider.terminate).mockReturnValue({
        andThen: vi.fn().mockReturnValue(ok(undefined).toAsync()),
      } as never);

      const { terminate } = await import("@/actions/memo");

      await expect(terminate("01JTEST000000000000000")).rejects.toThrow(
        "認証が必要です",
      );

      expect(AdminMemoWorkflowProvider.terminate).not.toHaveBeenCalled();
    });

    it("未認証の場合 search は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminMemoWorkflowProvider } = await import(
        "@/providers/workflows/memo"
      );
      vi.mocked(AdminMemoWorkflowProvider.search).mockReturnValue(
        ok([]).toAsync(),
      );

      const { search } = await import("@/actions/memo");

      await expect(
        search({
          keyword: "",
          status: "draft",
          tags: [],
        }),
      ).rejects.toThrow("認証が必要です");

      expect(AdminMemoWorkflowProvider.search).not.toHaveBeenCalled();
    });
  });

  describe("series actions", () => {
    it("未認証の場合 persist は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminSeriesWorkflowProvider } = await import(
        "@/providers/workflows/series"
      );
      vi.mocked(AdminSeriesWorkflowProvider.persist).mockReturnValue(
        ok(undefined).toAsync(),
      );

      const { persist } = await import("@/actions/series");

      await expect(
        persist({
          identifier: "01JTEST000000000000000",
          title: "Test",
          slug: "test",
          description: "test",
          status: "draft",
          chapters: [],
          timeline: { createdAt: new Date(), updatedAt: new Date() },
        }),
      ).rejects.toThrow("認証が必要です");

      expect(AdminSeriesWorkflowProvider.persist).not.toHaveBeenCalled();
    });

    it("未認証の場合 terminate は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminSeriesWorkflowProvider } = await import(
        "@/providers/workflows/series"
      );
      vi.mocked(AdminSeriesWorkflowProvider.terminate).mockReturnValue(
        ok(undefined).toAsync(),
      );

      const { terminate } = await import("@/actions/series");

      await expect(terminate("01JTEST000000000000000")).rejects.toThrow(
        "認証が必要です",
      );

      expect(AdminSeriesWorkflowProvider.terminate).not.toHaveBeenCalled();
    });
  });

  describe("document actions", () => {
    it("未認証の場合 persistPrivacyPolicy は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminDocumentWorkflowProvider } = await import(
        "@/providers/workflows/document"
      );
      vi.mocked(AdminDocumentWorkflowProvider.PersistPrivatePolicy).mockReturnValue(
        ok(undefined).toAsync(),
      );

      const { persistPrivacyPolicy } = await import("@/actions/document");

      await expect(
        persistPrivacyPolicy({
          content: "test",
          effectiveDate: new Date(),
        }),
      ).rejects.toThrow("認証が必要です");

      expect(
        AdminDocumentWorkflowProvider.PersistPrivatePolicy,
      ).not.toHaveBeenCalled();
    });
  });

  describe("common actions", () => {
    it("未認証の場合 uploadImage は UnauthenticatedHttpError を throw する", async () => {
      const { isAdmin } = await import("@/actions/auth");
      vi.mocked(isAdmin).mockResolvedValue(false);

      const { AdminImageUploaderProvider } = await import(
        "@/providers/infrastructure/storage"
      );
      vi.mocked(AdminImageUploaderProvider.firebaseAdmin.upload).mockReturnValue(
        ok("https://example.com/image.png").toAsync(),
      );

      const { uploadImage } = await import("@/actions/common");

      await expect(
        uploadImage(new Blob(["test"]), "images/articles/test.png"),
      ).rejects.toThrow("認証が必要です");

      expect(AdminImageUploaderProvider.firebaseAdmin.upload).not.toHaveBeenCalled();
    });
  });
});
