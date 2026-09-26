/**
 * @vitest-environment node
 */
import { beforeEach, describe, expect, it, vi } from "vitest";
import { ok } from "@shared/aspects/result";
import { searchByToken } from "../../src/actions/search-token";
import { ArticleWorkflowProvider } from "../../src/providers/workflows/article";
import { ReaderSearchTokenWorkflowProvider } from "../../src/providers/workflows/search-token";
import { articleSchema } from "@shared/domains/articles";

vi.mock("next/cache", () => ({
  unstable_cache: (callback: () => Promise<unknown>) => callback,
}));
vi.mock("@/providers/workflows/article", () => ({
  ArticleWorkflowProvider: { search: vi.fn() },
}));
vi.mock("@/providers/workflows/search-token", () => ({
  ReaderSearchTokenWorkflowProvider: { search: vi.fn() },
}));

const baseCriteria = {
  freeWord: "ÉCOLE",
  tags: null,
  type: null,
  sortBy: null,
  order: null,
  limit: 10,
};

describe("Reader search", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.mocked(ArticleWorkflowProvider.search).mockReturnValue(ok([]).toAsync());
    vi.mocked(ReaderSearchTokenWorkflowProvider.search).mockReturnValue(ok([]).toAsync());
  });

  it("記事検索を Article API のワークフローへ渡す", async () => {
    expect(await searchByToken(baseCriteria)).toEqual([]);
    expect(ArticleWorkflowProvider.search).toHaveBeenCalledWith({
      payload: {
        freeWord: "ÉCOLE",
        tags: null,
        status: "published",
      },
      now: expect.any(Date),
    });
  });

  it("Article API の記事を検索結果へ含める", async () => {
    const publishedAt = new Date("2026-09-26T00:00:00Z");
    const article = articleSchema.parse({
      identifier: "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      title: "ÉCOLE",
      content: "Article body",
      excerpt: "Article excerpt",
      slug: "ecole",
      status: "published",
      tags: [],
      images: [],
      publishedAt,
      timeline: { createdAt: publishedAt, updatedAt: publishedAt },
    });
    vi.mocked(ArticleWorkflowProvider.search).mockReturnValue(ok([article]).toAsync());

    const found = await searchByToken(baseCriteria);
    expect(found).toHaveLength(1);
    expect(found[0].identifier).toBe(article.identifier);
    expect(found[0].publishedAt).toEqual(publishedAt);
  });

  it("Memo 指定時は Article API を呼ばない", async () => {
    expect(await searchByToken({ ...baseCriteria, type: "memo" })).toEqual([]);
    expect(ArticleWorkflowProvider.search).not.toHaveBeenCalled();
  });

  it("Article 指定時は旧 SearchToken 経路を呼ばない", async () => {
    expect(await searchByToken({ ...baseCriteria, type: "article" })).toEqual([]);
    expect(ReaderSearchTokenWorkflowProvider.search).not.toHaveBeenCalled();
    expect(ArticleWorkflowProvider.search).toHaveBeenCalledOnce();
  });

  it("条件がない初期画面で全記事を取得しない", async () => {
    expect(await searchByToken({ ...baseCriteria, freeWord: null })).toEqual([]);
    expect(ArticleWorkflowProvider.search).not.toHaveBeenCalled();
  });
});
