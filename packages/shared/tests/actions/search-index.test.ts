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

vi.mock("@shared/components/global/next-error", () => ({
  unwrapForNextJs: vi.fn(),
}));

vi.mock("@shared/providers/workflows/search-index", () => ({
  SearchIndexWorkflowProvider: {
    search: vi.fn(),
  },
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
}> = {}) {
  return {
    freeWord: null,
    tags: null,
    type: null,
    sortBy: null,
    order: null,
    ...overrides,
  };
}

describe("search-index actions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  describe("search", () => {
    it("検索条件に一致するコンテンツ一覧を取得できる", async () => {
      const articles = Forger(ArticleMold).forgeMultiWithSeed(2, 1);
      const memos = Forger(MemoMold).forgeMultiWithSeed(2, 2);
      const seriesList = Forger(SeriesMold).forgeMultiWithSeed(2, 3);
      const mixedResults = [...articles, ...memos, ...seriesList];
      const unwrapForNextJs = await setupUnwrapWithResolvedValue(mixedResults);

      const { search } = await import("@shared/actions/search-index");
      const result = await search(createSearchCondition());

      expect(result).toEqual(mixedResults);
      expect(unwrapForNextJs).toHaveBeenCalled();
    });

    it("freeWordで検索できる", async () => {
      const articles = Forger(ArticleMold).forgeMultiWithSeed(2, 1);
      await setupUnwrapWithResolvedValue(articles);

      const { search } = await import("@shared/actions/search-index");
      const result = await search(createSearchCondition({ freeWord: "test" }));

      expect(result).toEqual(articles);
    });

    it("tagsで検索できる", async () => {
      const memos = Forger(MemoMold).forgeMultiWithSeed(2, 1);
      await setupUnwrapWithResolvedValue(memos);

      const { search } = await import("@shared/actions/search-index");
      const result = await search(createSearchCondition({ tags: ["tag1", "tag2"] }));

      expect(result).toEqual(memos);
    });

    it("検索条件に一致するコンテンツがない場合は空配列を返す", async () => {
      await setupUnwrapWithResolvedValue([]);

      const { search } = await import("@shared/actions/search-index");
      const result = await search(createSearchCondition({ freeWord: "non-existent" }));

      expect(result).toEqual([]);
    });

    it("検索エラーの場合はエラーがスローされる", async () => {
      await setupUnwrapWithRejectedValue(
        unexpectedError("Search failed", new Error("Database error"))
      );

      const { search } = await import("@shared/actions/search-index");

      await expect(search(createSearchCondition())).rejects.toThrow();
    });
  });
});
