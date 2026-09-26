/** @vitest-environment node */
import { beforeEach, describe, expect, it, vi } from "vitest";
import { slugSchema } from "@shared/domains/common";
import { ok } from "@shared/aspects/result";
import { isUnexpectedError } from "@shared/aspects/error";

const mocks = vi.hoisted(() => ({
  getCloudflareContext: vi.fn<() => Promise<{ env: Record<string, unknown> }>>(),
  fallbackFind: vi.fn(),
  fallbackSearch: vi.fn(),
}));

vi.mock("@opennextjs/cloudflare", () => ({
  getCloudflareContext: mocks.getCloudflareContext,
}));

vi.mock("@/providers/infrastructure/firebase-select", () => ({
  ReaderFirestoreProvider: { instance: {}, operations: {} },
}));

vi.mock("@shared/infrastructures/articles", () => ({
  FirebaseArticleRepository: () => ({
    findBySlug: mocks.fallbackFind,
    search: mocks.fallbackSearch,
  }),
}));

describe("Reader Article source selection", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.stubEnv("BUILD_TARGET", "");
    mocks.fallbackFind.mockReturnValue(ok("firebase").toAsync());
  });

  it("uses ARTICLE_API at runtime even without a build-target variable", async () => {
    const fetch = vi.fn(async () => Response.json({
      identifier: "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      phase: "published",
      title: "Haskell syntax",
      body: "# Syntax",
      slug: "haskell-syntax",
      excerpt: "A syntax guide.",
      tags: [],
      images: [],
      createdAt: "2026-01-01T00:00:00Z",
      updatedAt: "2026-01-02T00:00:00Z",
      publishedAt: "2026-01-03T00:00:00Z",
    }));
    mocks.getCloudflareContext.mockResolvedValue({ env: { ARTICLE_API: { fetch } } });
    const { ReaderArticleRepositoryProvider } = await import("@/providers/infrastructure/articles");

    const article = await ReaderArticleRepositoryProvider.current
      .findBySlug(slugSchema.parse("haskell-syntax"))
      .unwrap();

    expect(article.title).toBe("Haskell syntax");
    expect(fetch).toHaveBeenCalledOnce();
    expect(mocks.fallbackFind).not.toHaveBeenCalled();
  });

  it("uses Firebase only when no Article binding is configured", async () => {
    mocks.getCloudflareContext.mockResolvedValue({ env: {} });
    const { ReaderArticleRepositoryProvider } = await import("@/providers/infrastructure/articles");

    const article = await ReaderArticleRepositoryProvider.current
      .findBySlug(slugSchema.parse("haskell-syntax"))
      .unwrap();

    expect(article).toBe("firebase");
    expect(mocks.fallbackFind).toHaveBeenCalledOnce();
  });

  it("does not silently fall back for a malformed configured binding", async () => {
    mocks.getCloudflareContext.mockResolvedValue({ env: { ARTICLE_API: {} } });
    const { ReaderArticleRepositoryProvider } = await import("@/providers/infrastructure/articles");

    const error = await ReaderArticleRepositoryProvider.current
      .findBySlug(slugSchema.parse("haskell-syntax"))
      .match({ ok: () => null, err: (value) => value });

    expect(isUnexpectedError(error)).toBe(true);
    expect(mocks.fallbackFind).not.toHaveBeenCalled();
  });

  it("does not use Firebase when Cloudflare context fails in a Cloudflare build", async () => {
    vi.stubEnv("BUILD_TARGET", "cloudflare");
    mocks.getCloudflareContext.mockRejectedValue(new Error("Cloudflare context unavailable"));
    const { ReaderArticleRepositoryProvider } = await import("@/providers/infrastructure/articles");

    const error = await ReaderArticleRepositoryProvider.current
      .findBySlug(slugSchema.parse("haskell-syntax"))
      .match({ ok: () => null, err: (value) => value });

    expect(isUnexpectedError(error)).toBe(true);
    expect(mocks.fallbackFind).not.toHaveBeenCalled();
  });
});
