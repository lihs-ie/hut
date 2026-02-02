import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  contentTypeSchema,
  ContentType,
  contentSchema,
  searchReferenceIdentifierSchema,
  equalSearchReferenceIdentifier,
  searchReferenceSchema,
  validateSearchReference,
} from "@shared/domains/search-token/reference";
import {
  SearchReferenceMold,
  SearchReferenceIdentifierMold,
} from "../../support/molds/domains/search-token";
import { ArticleIdentifierMold } from "../../support/molds/domains/article";
import { MemoIdentifierMold } from "../../support/molds/domains/memo";
import { SeriesIdentifierMold } from "../../support/molds/domains/series";
import { DateMold } from "../../support/molds/domains/common/date";
import { describeEnumSchema } from "../../support/helpers";

describe("domains/search-token/reference", () => {
  describe("contentTypeSchema", () => {
    describeEnumSchema("ContentType", contentTypeSchema, ["article", "memo", "series"], {
      ARTICLE: ContentType.ARTICLE,
      MEMO: ContentType.MEMO,
      SERIES: ContentType.SERIES,
    });

    it("'all' は無効（search-token/referenceでは使用しない）", () => {
      const result = contentTypeSchema.safeParse("all");
      expect(result.success).toBe(false);
    });
  });

  describe("contentSchema", () => {
    describe("有効なContentの検証", () => {
      it("ArticleIdentifierは有効", () => {
        const result = contentSchema.safeParse(Forger(ArticleIdentifierMold).forge());
        expect(result.success).toBe(true);
      });

      it("MemoIdentifierは有効", () => {
        const result = contentSchema.safeParse(Forger(MemoIdentifierMold).forge());
        expect(result.success).toBe(true);
      });

      it("SeriesIdentifierは有効", () => {
        const result = contentSchema.safeParse(Forger(SeriesIdentifierMold).forge());
        expect(result.success).toBe(true);
      });
    });

    describe("無効なContentの検証", () => {
      it("無効なULIDは無効", () => {
        const result = contentSchema.safeParse("invalid-ulid");
        expect(result.success).toBe(false);
      });

      it("空文字列は無効", () => {
        const result = contentSchema.safeParse("");
        expect(result.success).toBe(false);
      });
    });
  });

  describe("searchReferenceIdentifierSchema", () => {
    describe("有効なSearchReferenceIdentifierの検証", () => {
      it("type=articleでArticleIdentifierは有効", () => {
        const result = searchReferenceIdentifierSchema.safeParse({
          type: "article",
          content: Forger(ArticleIdentifierMold).forge(),
        });
        expect(result.success).toBe(true);
      });

      it("type=memoでMemoIdentifierは有効", () => {
        const result = searchReferenceIdentifierSchema.safeParse({
          type: "memo",
          content: Forger(MemoIdentifierMold).forge(),
        });
        expect(result.success).toBe(true);
      });

      it("type=seriesでSeriesIdentifierは有効", () => {
        const result = searchReferenceIdentifierSchema.safeParse({
          type: "series",
          content: Forger(SeriesIdentifierMold).forge(),
        });
        expect(result.success).toBe(true);
      });

      it("Forgerで生成したidentifierは有効", () => {
        const result = searchReferenceIdentifierSchema.safeParse(
          Forger(SearchReferenceIdentifierMold).forge()
        );
        expect(result.success).toBe(true);
      });
    });

    describe("無効なSearchReferenceIdentifierの検証", () => {
      it("typeが無効な場合は無効", () => {
        const result = searchReferenceIdentifierSchema.safeParse({
          type: "invalid",
          content: Forger(ArticleIdentifierMold).forge(),
        });
        expect(result.success).toBe(false);
      });

      it("contentが無効な場合は無効", () => {
        const result = searchReferenceIdentifierSchema.safeParse({
          type: "article",
          content: "invalid-ulid",
        });
        expect(result.success).toBe(false);
      });

      it("typeが欠けている場合は無効", () => {
        const result = searchReferenceIdentifierSchema.safeParse({
          content: Forger(ArticleIdentifierMold).forge(),
        });
        expect(result.success).toBe(false);
      });

      it("contentが欠けている場合は無効", () => {
        const result = searchReferenceIdentifierSchema.safeParse({ type: "article" });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("equalSearchReferenceIdentifier", () => {
    it("同じtypeとcontentの場合はtrueを返す", () => {
      const articleId = Forger(ArticleIdentifierMold).forge();
      const left = { type: ContentType.ARTICLE, content: articleId };
      const right = { type: ContentType.ARTICLE, content: articleId };

      expect(equalSearchReferenceIdentifier(left, right)).toBe(true);
    });

    it("typeが異なる場合はfalseを返す", () => {
      const id = Forger(ArticleIdentifierMold).forge();
      const left = { type: ContentType.ARTICLE, content: id };
      const right = { type: ContentType.MEMO, content: id };

      expect(equalSearchReferenceIdentifier(left, right)).toBe(false);
    });

    it("contentが異なる場合はfalseを返す", () => {
      const left = { type: ContentType.ARTICLE, content: Forger(ArticleIdentifierMold).forge() };
      const right = { type: ContentType.ARTICLE, content: Forger(ArticleIdentifierMold).forge() };

      expect(equalSearchReferenceIdentifier(left, right)).toBe(false);
    });

    it("異なるtypeと異なるcontentの場合はfalseを返す", () => {
      const left = { type: ContentType.ARTICLE, content: Forger(ArticleIdentifierMold).forge() };
      const right = { type: ContentType.MEMO, content: Forger(MemoIdentifierMold).forge() };

      expect(equalSearchReferenceIdentifier(left, right)).toBe(false);
    });
  });

  describe("searchReferenceSchema", () => {
    describe("有効なSearchReferenceの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = searchReferenceSchema.safeParse(Forger(SearchReferenceMold).forge());
        expect(result.success).toBe(true);
      });

      it("scoreが0でも有効", () => {
        const result = searchReferenceSchema.safeParse({
          identifier: Forger(SearchReferenceIdentifierMold).forge(),
          score: 0,
          updatedAt: Forger(DateMold).forge(),
        });
        expect(result.success).toBe(true);
      });

      it("scoreが大きな数値でも有効", () => {
        const result = searchReferenceSchema.safeParse({
          identifier: Forger(SearchReferenceIdentifierMold).forge(),
          score: 999999,
          updatedAt: Forger(DateMold).forge(),
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なSearchReferenceの検証", () => {
      it("identifierが無効な場合は無効", () => {
        const result = searchReferenceSchema.safeParse({
          identifier: { type: "invalid", content: "invalid" },
          score: 5,
          updatedAt: Forger(DateMold).forge(),
        });
        expect(result.success).toBe(false);
      });

      it("scoreが負の場合は無効", () => {
        const result = searchReferenceSchema.safeParse({
          identifier: Forger(SearchReferenceIdentifierMold).forge(),
          score: -1,
          updatedAt: Forger(DateMold).forge(),
        });
        expect(result.success).toBe(false);
      });

      it("updatedAtがDateでない場合は無効", () => {
        const result = searchReferenceSchema.safeParse({
          identifier: Forger(SearchReferenceIdentifierMold).forge(),
          score: 5,
          updatedAt: "2024-01-01",
        });
        expect(result.success).toBe(false);
      });

      it("updatedAtが欠けている場合は無効", () => {
        const result = searchReferenceSchema.safeParse({
          identifier: Forger(SearchReferenceIdentifierMold).forge(),
          score: 5,
        });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateSearchReference", () => {
    it("有効なSearchReferenceでokを返す", () => {
      const result = validateSearchReference({
        identifier: { type: "article", content: Forger(ArticleIdentifierMold).forge() },
        score: 5,
        updatedAt: Forger(DateMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なSearchReferenceでerrを返す", () => {
      const result = validateSearchReference({
        identifier: { type: "invalid", content: "invalid" },
        score: -1,
        updatedAt: Forger(DateMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });

    it("scoreが負の場合はerrを返す", () => {
      const result = validateSearchReference({
        identifier: { type: "article", content: Forger(ArticleIdentifierMold).forge() },
        score: -1,
        updatedAt: Forger(DateMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });
  });
});
