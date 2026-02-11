import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  searchKeywordSchema,
  searchRecordIdentifierSchema,
  searchRecordSchema,
  criteriaSchema,
  validateSearchRecord,
  validateCriteria,
  validateSearchRecordIdentifier,
  validateSearchKeyword,
} from "@shared/domains/analytics/search-record";
import {
  SearchRecordIdentifierMold,
  SearchKeywordMold,
  SearchRecordMold,
  DateKeyMold,
  TagIdentifierForAnalyticsMold,
} from "../../support/molds/domains/analytics/search-record";
import {
  describeIdentifierSchema,
} from "../../support/helpers";

describe("domains/analytics/search-record", () => {
  // -----------------------------------------------------------------------
  // searchRecordIdentifierSchema
  // -----------------------------------------------------------------------
  describe("searchRecordIdentifierSchema", () => {
    describeIdentifierSchema(
      "SearchRecordIdentifier",
      searchRecordIdentifierSchema,
      () => Forger(SearchRecordIdentifierMold).forge(),
      (count) => Forger(SearchRecordIdentifierMold).forgeMulti(count)
    );
  });

  // -----------------------------------------------------------------------
  // validateSearchRecordIdentifier
  // -----------------------------------------------------------------------
  describe("validateSearchRecordIdentifier", () => {
    it("有効なULIDでokを返す", () => {
      const result = validateSearchRecordIdentifier(
        Forger(SearchRecordIdentifierMold).forge()
      );
      expect(result.isOk).toBe(true);
    });

    it("無効な文字列でerrを返す", () => {
      const result = validateSearchRecordIdentifier("invalid");
      expect(result.isErr).toBe(true);
    });

    it("空文字列でerrを返す", () => {
      const result = validateSearchRecordIdentifier("");
      expect(result.isErr).toBe(true);
    });
  });

  // -----------------------------------------------------------------------
  // searchKeywordSchema
  // -----------------------------------------------------------------------
  describe("searchKeywordSchema", () => {
    it("1文字以上の文字列は有効", () => {
      const result = searchKeywordSchema.safeParse("a");
      expect(result.success).toBe(true);
    });

    it("空文字列は無効", () => {
      const result = searchKeywordSchema.safeParse("");
      expect(result.success).toBe(false);
    });

    it("nullは無効", () => {
      const result = searchKeywordSchema.safeParse(null);
      expect(result.success).toBe(false);
    });

    it("undefinedは無効", () => {
      const result = searchKeywordSchema.safeParse(undefined);
      expect(result.success).toBe(false);
    });

    it("日本語キーワードは有効", () => {
      const result = searchKeywordSchema.safeParse("検索キーワード");
      expect(result.success).toBe(true);
    });

    it("長い文字列も有効", () => {
      const result = searchKeywordSchema.safeParse("a".repeat(500));
      expect(result.success).toBe(true);
    });
  });

  // -----------------------------------------------------------------------
  // validateSearchKeyword
  // -----------------------------------------------------------------------
  describe("validateSearchKeyword", () => {
    it("有効なキーワードでokを返す", () => {
      const result = validateSearchKeyword("React");
      expect(result.isOk).toBe(true);
    });

    it("空文字列でerrを返す", () => {
      const result = validateSearchKeyword("");
      expect(result.isErr).toBe(true);
    });
  });

  // -----------------------------------------------------------------------
  // searchRecordSchema
  // -----------------------------------------------------------------------
  describe("searchRecordSchema", () => {
    describe("有効なSearchRecordの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const record = Forger(SearchRecordMold).forge();
        const result = searchRecordSchema.safeParse(record);
        expect(result.success).toBe(true);
      });

      it("複数のSearchRecordが検証を通過する", () => {
        const records = Forger(SearchRecordMold).forgeMulti(5);
        for (const record of records) {
          const result = searchRecordSchema.safeParse(record);
          expect(result.success).toBe(true);
        }
      });

      it("tagsがnullの場合も有効", () => {
        const result = searchRecordSchema.safeParse({
          identifier: Forger(SearchRecordIdentifierMold).forge(),
          dateKey: Forger(DateKeyMold).forge(),
          keyword: Forger(SearchKeywordMold).forge(),
          resultCount: 5,
          tags: null,
          contentType: "article",
          createdAt: new Date(),
        });
        expect(result.success).toBe(true);
      });

      it("contentTypeがnullの場合も有効", () => {
        const result = searchRecordSchema.safeParse({
          identifier: Forger(SearchRecordIdentifierMold).forge(),
          dateKey: Forger(DateKeyMold).forge(),
          keyword: Forger(SearchKeywordMold).forge(),
          resultCount: 0,
          tags: [Forger(TagIdentifierForAnalyticsMold).forge()],
          contentType: null,
          createdAt: new Date(),
        });
        expect(result.success).toBe(true);
      });

      it("resultCountが0の場合も有効", () => {
        const result = searchRecordSchema.safeParse({
          identifier: Forger(SearchRecordIdentifierMold).forge(),
          dateKey: Forger(DateKeyMold).forge(),
          keyword: Forger(SearchKeywordMold).forge(),
          resultCount: 0,
          tags: null,
          contentType: null,
          createdAt: new Date(),
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なSearchRecordの検証", () => {
      const createRecordWithOverrides = (
        overrides: Record<string, unknown>
      ) => ({
        identifier: Forger(SearchRecordIdentifierMold).forge(),
        dateKey: Forger(DateKeyMold).forge(),
        keyword: Forger(SearchKeywordMold).forge(),
        resultCount: 5,
        tags: null,
        contentType: null,
        createdAt: new Date(),
        ...overrides,
      });

      it("identifierが無効な場合は無効", () => {
        const result = searchRecordSchema.safeParse(
          createRecordWithOverrides({ identifier: "invalid" })
        );
        expect(result.success).toBe(false);
      });

      it("dateKeyが不正な形式の場合は無効", () => {
        const result = searchRecordSchema.safeParse(
          createRecordWithOverrides({ dateKey: "2024/01/01" })
        );
        expect(result.success).toBe(false);
      });

      it("keywordが空文字の場合は無効", () => {
        const result = searchRecordSchema.safeParse(
          createRecordWithOverrides({ keyword: "" })
        );
        expect(result.success).toBe(false);
      });

      it("resultCountが負の場合は無効", () => {
        const result = searchRecordSchema.safeParse(
          createRecordWithOverrides({ resultCount: -1 })
        );
        expect(result.success).toBe(false);
      });

      it("resultCountが小数の場合は無効", () => {
        const result = searchRecordSchema.safeParse(
          createRecordWithOverrides({ resultCount: 1.5 })
        );
        expect(result.success).toBe(false);
      });

      it("createdAtがDateでない場合は無効", () => {
        const result = searchRecordSchema.safeParse(
          createRecordWithOverrides({ createdAt: "not-a-date" })
        );
        expect(result.success).toBe(false);
      });

      it("contentTypeが不正な値の場合は無効", () => {
        const result = searchRecordSchema.safeParse(
          createRecordWithOverrides({ contentType: "invalid" })
        );
        expect(result.success).toBe(false);
      });

      it("tagsに不正なULIDが含まれる場合は無効", () => {
        const result = searchRecordSchema.safeParse(
          createRecordWithOverrides({ tags: ["invalid-ulid"] })
        );
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = searchRecordSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = searchRecordSchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });
    });
  });

  // -----------------------------------------------------------------------
  // validateSearchRecord
  // -----------------------------------------------------------------------
  describe("validateSearchRecord", () => {
    it("有効なUnvalidatedSearchRecordでokを返す", () => {
      const result = validateSearchRecord({
        identifier: Forger(SearchRecordIdentifierMold).forge(),
        dateKey: Forger(DateKeyMold).forge(),
        keyword: "React hooks",
        resultCount: 10,
        tags: [Forger(TagIdentifierForAnalyticsMold).forge()],
        contentType: "article",
        createdAt: new Date(),
      });
      expect(result.isOk).toBe(true);
    });

    it("tagsがnullでもokを返す", () => {
      const result = validateSearchRecord({
        identifier: Forger(SearchRecordIdentifierMold).forge(),
        dateKey: Forger(DateKeyMold).forge(),
        keyword: "Next.js",
        resultCount: 0,
        tags: null,
        contentType: null,
        createdAt: new Date(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedSearchRecordでerrを返す", () => {
      const result = validateSearchRecord({
        identifier: "invalid",
        dateKey: "bad-date",
        keyword: "",
        resultCount: -1,
        tags: null,
        contentType: null,
        createdAt: new Date(),
      });
      expect(result.isErr).toBe(true);
    });

    it("errの場合にValidationError配列を返す", () => {
      const result = validateSearchRecord({
        identifier: "invalid",
        dateKey: "bad-date",
        keyword: "",
        resultCount: -1,
        tags: null,
        contentType: null,
        createdAt: new Date(),
      });
      expect(result.isErr).toBe(true);
      if (result.isErr) {
        const errors = result.unwrapError();
        expect(Array.isArray(errors)).toBe(true);
        expect(errors.length).toBeGreaterThan(0);
      }
    });
  });

  // -----------------------------------------------------------------------
  // criteriaSchema
  // -----------------------------------------------------------------------
  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("全フィールドがnullish(undefined)で有効", () => {
        const result = criteriaSchema.safeParse({});
        expect(result.success).toBe(true);
      });

      it("全フィールドがnullで有効", () => {
        const result = criteriaSchema.safeParse({
          dateRange: null,
          keyword: null,
          hasResults: null,
        });
        expect(result.success).toBe(true);
      });

      it("dateRangeを指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          dateRange: {
            start: new Date("2024-01-01"),
            end: new Date("2024-12-31"),
          },
        });
        expect(result.success).toBe(true);
      });

      it("keywordを指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          keyword: "React",
        });
        expect(result.success).toBe(true);
      });

      it("hasResultsをtrueで指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          hasResults: true,
        });
        expect(result.success).toBe(true);
      });

      it("hasResultsをfalseで指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          hasResults: false,
        });
        expect(result.success).toBe(true);
      });

      it("全フィールドを指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          dateRange: {
            start: new Date("2024-01-01"),
            end: new Date("2024-06-30"),
          },
          keyword: "TypeScript",
          hasResults: true,
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("dateRangeのendがstartより前の場合は無効", () => {
        const result = criteriaSchema.safeParse({
          dateRange: {
            start: new Date("2024-12-31"),
            end: new Date("2024-01-01"),
          },
        });
        expect(result.success).toBe(false);
      });

      it("keywordが空文字の場合は無効", () => {
        const result = criteriaSchema.safeParse({
          keyword: "",
        });
        expect(result.success).toBe(false);
      });

      it("hasResultsが文字列の場合は無効", () => {
        const result = criteriaSchema.safeParse({
          hasResults: "true",
        });
        expect(result.success).toBe(false);
      });
    });
  });

  // -----------------------------------------------------------------------
  // validateCriteria
  // -----------------------------------------------------------------------
  describe("validateCriteria", () => {
    it("有効なCriteriaでokを返す", () => {
      const result = validateCriteria({
        keyword: "React",
        hasResults: true,
      });
      expect(result.isOk).toBe(true);
    });

    it("空のCriteriaでもokを返す", () => {
      const result = validateCriteria({});
      expect(result.isOk).toBe(true);
    });

    it("全フィールドnullでもokを返す", () => {
      const result = validateCriteria({
        dateRange: null,
        keyword: null,
        hasResults: null,
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なCriteriaでerrを返す", () => {
      const result = validateCriteria({
        keyword: "",
      });
      expect(result.isErr).toBe(true);
    });

    it("dateRangeが逆転している場合にerrを返す", () => {
      const result = validateCriteria({
        dateRange: {
          start: new Date("2024-12-31"),
          end: new Date("2024-01-01"),
        },
      });
      expect(result.isErr).toBe(true);
    });
  });
});
