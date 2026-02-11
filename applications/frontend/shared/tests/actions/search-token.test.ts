/**
 * @vitest-environment node
 */
import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../support/molds/domains/article/common";
import { MemoMold } from "../support/molds/domains/memo/common";
import { SeriesMold } from "../support/molds/domains/series/common";
import { unexpectedError } from "@shared/aspects/error";
import {
  setupUnwrapWithResolvedValue,
  setupUnwrapWithRejectedValue,
} from "../support/helpers";

vi.mock("next/cache", () => ({
  unstable_cache: <T extends (...args: unknown[]) => unknown>(fn: T) => fn,
}));

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: vi.fn(),
}));

vi.mock("@shared/providers/workflows/search-token", () => ({
  SearchTokenWorkflowProvider: {
    search: vi.fn(),
  },
}));

vi.mock("@shared/actions/search-log", () => ({
  recordSearchLog: vi.fn(),
}));

/**
 * 共通の検索条件オブジェクトを生成する
 */
function createSearchCondition(overrides: Partial<{
  freeWord: string | null;
  tags: string[] | null;
  type: string | null;
  sortBy: string | null;
  order: string | null;
  limit: number | null;
}> = {}) {
  return {
    freeWord: null,
    tags: null,
    type: null,
    sortBy: null,
    order: null,
    limit: null,
    ...overrides,
  };
}

describe("search-token actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  describe("searchByToken", () => {
    it("検索条件に一致するコンテンツ一覧を取得できる", async () => {
      const articles = Forger(ArticleMold).forgeMultiWithSeed(2, 1);
      const memos = Forger(MemoMold).forgeMultiWithSeed(2, 2);
      const seriesList = Forger(SeriesMold).forgeMultiWithSeed(2, 3);
      const mixedResults = [...articles, ...memos, ...seriesList];
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(mixedResults);

      const { searchByToken } = await import("@shared/actions/search-token");
      const result = await searchByToken(createSearchCondition());

      expect(result).toEqual(mixedResults);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("freeWordで検索できる", async () => {
      const articles = Forger(ArticleMold).forgeMultiWithSeed(2, 1);
      await setupUnwrapWithResolvedValue(articles);

      const { searchByToken } = await import("@shared/actions/search-token");
      const result = await searchByToken(createSearchCondition({ freeWord: "test" }));

      expect(result).toEqual(articles);
    });

    it("tagsで検索できる", async () => {
      const memos = Forger(MemoMold).forgeMultiWithSeed(2, 1);
      await setupUnwrapWithResolvedValue(memos);

      const { searchByToken } = await import("@shared/actions/search-token");
      const result = await searchByToken(createSearchCondition({ tags: ["tag1", "tag2"] }));

      expect(result).toEqual(memos);
    });

    it("limitで結果数を制限できる", async () => {
      const articles = Forger(ArticleMold).forgeMultiWithSeed(5, 1);
      const limitedArticles = articles.slice(0, 3);
      await setupUnwrapWithResolvedValue(limitedArticles);

      const { searchByToken } = await import("@shared/actions/search-token");
      const result = await searchByToken(createSearchCondition({ limit: 3 }));

      expect(result).toEqual(limitedArticles);
      expect(result.length).toBe(3);
    });

    it("検索条件に一致するコンテンツがない場合は空配列を返す", async () => {
      await setupUnwrapWithResolvedValue([]);

      const { searchByToken } = await import("@shared/actions/search-token");
      const result = await searchByToken(createSearchCondition({ freeWord: "non-existent" }));

      expect(result).toEqual([]);
    });

    it("検索エラーの場合はエラーがスローされる", async () => {
      await setupUnwrapWithRejectedValue(
        unexpectedError("Search failed", new Error("Database error"))
      );

      const { searchByToken } = await import("@shared/actions/search-token");

      await expect(searchByToken(createSearchCondition())).rejects.toThrow();
    });

    it("キャッシュからのDate文字列が正しくDateオブジェクトに復元される", async () => {
      // unstable_cacheはJSON.stringify/parseを行うため、DateがISO文字列になる
      // この問題をシミュレートするため、ISO文字列を含むデータを準備
      const article = Forger(ArticleMold).forgeWithSeed(1);
      const serializedArticle = JSON.parse(JSON.stringify(article));

      // シリアライズ後、timeline.createdAtとupdatedAtは文字列になっている
      expect(typeof serializedArticle.timeline.createdAt).toBe("string");
      expect(typeof serializedArticle.timeline.updatedAt).toBe("string");

      await setupUnwrapWithResolvedValue([serializedArticle]);

      const { searchByToken } = await import("@shared/actions/search-token");
      const result = await searchByToken(createSearchCondition());

      // Date復元後、Dateオブジェクトになっていることを確認
      expect(result[0].timeline.createdAt).toBeInstanceOf(Date);
      expect(result[0].timeline.updatedAt).toBeInstanceOf(Date);
    });

    it("キャッシュからのMemoのentry.createdAtが正しくDateオブジェクトに復元される", async () => {
      const memo = Forger(MemoMold).forgeWithSeed(1);
      const serializedMemo = JSON.parse(JSON.stringify(memo));

      // シリアライズ後、entries[].createdAtも文字列になっている
      if (serializedMemo.entries.length > 0) {
        expect(typeof serializedMemo.entries[0].createdAt).toBe("string");
      }

      await setupUnwrapWithResolvedValue([serializedMemo]);

      const { searchByToken } = await import("@shared/actions/search-token");
      const result = await searchByToken(createSearchCondition());

      // Date復元後、Dateオブジェクトになっていることを確認
      expect(result[0].timeline.createdAt).toBeInstanceOf(Date);
      if ((result[0] as typeof memo).entries.length > 0) {
        expect((result[0] as typeof memo).entries[0].createdAt).toBeInstanceOf(Date);
      }
    });

    it("キャッシュからのSeriesのchapter.timelineが正しくDateオブジェクトに復元される", async () => {
      const series = Forger(SeriesMold).forgeWithSeed(1);
      const serializedSeries = JSON.parse(JSON.stringify(series));

      // シリアライズ後、chapters[].timeline.createdAtも文字列になっている
      if (serializedSeries.chapters.length > 0) {
        expect(typeof serializedSeries.chapters[0].timeline.createdAt).toBe("string");
      }

      await setupUnwrapWithResolvedValue([serializedSeries]);

      const { searchByToken } = await import("@shared/actions/search-token");
      const result = await searchByToken(createSearchCondition());

      // Date復元後、Dateオブジェクトになっていることを確認
      expect(result[0].timeline.createdAt).toBeInstanceOf(Date);
      if ((result[0] as typeof series).chapters.length > 0) {
        expect((result[0] as typeof series).chapters[0].timeline.createdAt).toBeInstanceOf(Date);
      }
    });
  });
});
