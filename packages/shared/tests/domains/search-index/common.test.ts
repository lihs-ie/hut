import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  contentTypeSchema,
  ContentType,
  sortSchema,
  Sort,
  orderSchema,
  Order,
  criteriaSchema,
  validateCriteria,
  titleSchema,
  excerptSchema,
  slugSchema,
  searchIndexIdentifierSchema,
  searchIndexSchema,
  validateSearchIndex,
  extractReferences,
} from "@shared/domains/search-index";
import {
  SearchIndexMold,
  SearchIndexIdentifierMold,
  SearchIndexTitleMold,
  SearchIndexExcerptMold,
  CriteriaMold,
} from "../../support/molds/domains/search-index";
import { TimelineMold } from "../../support/molds/domains/common/date";
import { TagIdentifierMold } from "../../support/molds/domains/attributes/tag";
import { ArticleIdentifierMold } from "../../support/molds/domains/article";
import { MemoIdentifierMold } from "../../support/molds/domains/memo";
import { SeriesIdentifierMold } from "../../support/molds/domains/series";
import {
  describeIdentifierSchema,
  describeStringLengthSchema,
  describeEnumSchema,
} from "../../support/helpers";

describe("domains/search-index/common", () => {
  describe("contentTypeSchema", () => {
    describeEnumSchema("ContentType", contentTypeSchema, ["all", "article", "memo", "series"], {
      ALL: ContentType.ALL,
      ARTICLE: ContentType.ARTICLE,
      MEMO: ContentType.MEMO,
      SERIES: ContentType.SERIES,
    });
  });

  describe("sortSchema", () => {
    describeEnumSchema("Sort", sortSchema, ["latest", "newest", "oldest"], {
      LATEST: Sort.LATEST,
      NEWEST: Sort.NEWEST,
      OLDEST: Sort.OLDEST,
    });
  });

  describe("orderSchema", () => {
    describeEnumSchema("Order", orderSchema, ["asc", "desc"], {
      ASC: Order.ASC,
      DESC: Order.DESC,
    });
  });

  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("全てnullで有効", () => {
        const result = criteriaSchema.safeParse({
          freeWord: null,
          tags: null,
          type: null,
          sortBy: null,
          order: null,
        });
        expect(result.success).toBe(true);
      });

      it("全てのフィールドを指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          freeWord: "検索ワード",
          tags: Forger(TagIdentifierMold).forgeMulti(2),
          type: "article",
          sortBy: "latest",
          order: "desc",
        });
        expect(result.success).toBe(true);
      });

      it("Forgerで生成したCriteriaは有効", () => {
        const result = criteriaSchema.safeParse(Forger(CriteriaMold).forge());
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("freeWordが空文字列の場合は無効", () => {
        const result = criteriaSchema.safeParse({
          freeWord: "",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
        });
        expect(result.success).toBe(false);
      });

      it("freeWordが101文字以上の場合は無効", () => {
        const result = criteriaSchema.safeParse({
          freeWord: "a".repeat(101),
          tags: null,
          type: null,
          sortBy: null,
          order: null,
        });
        expect(result.success).toBe(false);
      });

      it("無効なtypeの場合は無効", () => {
        const result = criteriaSchema.safeParse({
          freeWord: null,
          tags: null,
          type: "invalid",
          sortBy: null,
          order: null,
        });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateCriteria", () => {
    it("有効なCriteriaでokを返す", () => {
      const result = validateCriteria({
        freeWord: "テスト",
        tags: null,
        type: "article",
        sortBy: "latest",
        order: "desc",
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なCriteriaでerrを返す", () => {
      const result = validateCriteria({
        freeWord: "",
        tags: null,
        type: null,
        sortBy: null,
        order: null,
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("titleSchema", () => {
    describeStringLengthSchema("タイトル", titleSchema, 1, 100);
  });

  describe("excerptSchema", () => {
    describeStringLengthSchema("抜粋", excerptSchema, 1, 300);
  });

  describe("slugSchema", () => {
    describeStringLengthSchema("Slug", slugSchema, 1, 100);
  });

  describe("searchIndexIdentifierSchema", () => {
    describeIdentifierSchema(
      "SearchIndexIdentifier",
      searchIndexIdentifierSchema,
      () => Forger(SearchIndexIdentifierMold).forge(),
      (count) => Forger(SearchIndexIdentifierMold).forgeMulti(count)
    );
  });

  describe("searchIndexSchema", () => {
    describe("有効なSearchIndexの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = searchIndexSchema.safeParse(Forger(SearchIndexMold).forge());
        expect(result.success).toBe(true);
      });

      it("type=articleでArticleIdentifierのreferenceは有効", () => {
        const result = searchIndexSchema.safeParse(
          Forger(SearchIndexMold).forge({
            type: ContentType.ARTICLE,
            reference: Forger(ArticleIdentifierMold).forge(),
          })
        );
        expect(result.success).toBe(true);
      });

      it("type=memoでMemoIdentifierのreferenceは有効", () => {
        const result = searchIndexSchema.safeParse(
          Forger(SearchIndexMold).forge({
            type: ContentType.MEMO,
            reference: Forger(MemoIdentifierMold).forge(),
          })
        );
        expect(result.success).toBe(true);
      });

      it("type=seriesでSeriesIdentifierのreferenceは有効", () => {
        const result = searchIndexSchema.safeParse(
          Forger(SearchIndexMold).forge({
            type: ContentType.SERIES,
            reference: Forger(SeriesIdentifierMold).forge(),
          })
        );
        expect(result.success).toBe(true);
      });
    });

    describe("無効なSearchIndexの検証", () => {
      const createSearchIndexWithOverrides = (overrides: Record<string, unknown>) => ({
        identifier: Forger(SearchIndexIdentifierMold).forge(),
        type: "article",
        title: Forger(SearchIndexTitleMold).forge(),
        excerpt: Forger(SearchIndexExcerptMold).forge(),
        tags: Forger(TagIdentifierMold).forgeMulti(2),
        reference: Forger(ArticleIdentifierMold).forge(),
        timeline: Forger(TimelineMold).forge(),
        ...overrides,
      });

      it("identifierが無効な場合は無効", () => {
        const result = searchIndexSchema.safeParse(
          createSearchIndexWithOverrides({ identifier: "invalid" })
        );
        expect(result.success).toBe(false);
      });

      it("titleが空の場合は無効", () => {
        const result = searchIndexSchema.safeParse(
          createSearchIndexWithOverrides({ title: "" })
        );
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateSearchIndex", () => {
    it("有効なSearchIndexでokを返す", () => {
      const result = validateSearchIndex({
        identifier: Forger(SearchIndexIdentifierMold).forge(),
        type: "article",
        title: "テストタイトル",
        excerpt: "テスト抜粋",
        tags: Forger(TagIdentifierMold).forgeMulti(2),
        reference: Forger(ArticleIdentifierMold).forge(),
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なSearchIndexでerrを返す", () => {
      const result = validateSearchIndex({
        identifier: "invalid",
        type: "invalid",
        title: "",
        excerpt: "",
        tags: [],
        reference: "invalid",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("extractReferences", () => {
    it("空の配列から空のタプルを返す", () => {
      const [articleIds, memoIds, seriesIds] = extractReferences([]);
      expect(articleIds).toHaveLength(0);
      expect(memoIds).toHaveLength(0);
      expect(seriesIds).toHaveLength(0);
    });

    it("Article型のインデックスからArticleIdentifierを抽出する", () => {
      const articleId = Forger(ArticleIdentifierMold).forge();
      const searchIndex = Forger(SearchIndexMold).forge({
        type: ContentType.ARTICLE,
        reference: articleId,
      });
      const [articleIds, memoIds, seriesIds] = extractReferences([searchIndex]);

      expect(articleIds).toHaveLength(1);
      expect(articleIds[0]).toBe(articleId);
      expect(memoIds).toHaveLength(0);
      expect(seriesIds).toHaveLength(0);
    });

    it("Memo型のインデックスからMemoIdentifierを抽出する", () => {
      const memoId = Forger(MemoIdentifierMold).forge();
      const searchIndex = Forger(SearchIndexMold).forge({
        type: ContentType.MEMO,
        reference: memoId,
      });
      const [articleIds, memoIds, seriesIds] = extractReferences([searchIndex]);

      expect(articleIds).toHaveLength(0);
      expect(memoIds).toHaveLength(1);
      expect(memoIds[0]).toBe(memoId);
      expect(seriesIds).toHaveLength(0);
    });

    it("Series型のインデックスからSeriesIdentifierを抽出する", () => {
      const seriesId = Forger(SeriesIdentifierMold).forge();
      const searchIndex = Forger(SearchIndexMold).forge({
        type: ContentType.SERIES,
        reference: seriesId,
      });
      const [articleIds, memoIds, seriesIds] = extractReferences([searchIndex]);

      expect(articleIds).toHaveLength(0);
      expect(memoIds).toHaveLength(0);
      expect(seriesIds).toHaveLength(1);
      expect(seriesIds[0]).toBe(seriesId);
    });

    it("混合した型のインデックスから正しくIDを抽出する", () => {
      const articleId1 = Forger(ArticleIdentifierMold).forge();
      const articleId2 = Forger(ArticleIdentifierMold).forge();
      const memoId = Forger(MemoIdentifierMold).forge();
      const seriesId = Forger(SeriesIdentifierMold).forge();

      const indices = [
        Forger(SearchIndexMold).forge({ type: ContentType.ARTICLE, reference: articleId1 }),
        Forger(SearchIndexMold).forge({ type: ContentType.MEMO, reference: memoId }),
        Forger(SearchIndexMold).forge({ type: ContentType.ARTICLE, reference: articleId2 }),
        Forger(SearchIndexMold).forge({ type: ContentType.SERIES, reference: seriesId }),
      ];

      const [articleIds, memoIds, seriesIds] = extractReferences(indices);

      expect(articleIds).toHaveLength(2);
      expect(articleIds).toContain(articleId1);
      expect(articleIds).toContain(articleId2);
      expect(memoIds).toHaveLength(1);
      expect(memoIds[0]).toBe(memoId);
      expect(seriesIds).toHaveLength(1);
      expect(seriesIds[0]).toBe(seriesId);
    });
  });
});
