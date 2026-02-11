import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  searchTokenTypeSchema,
  SearchTokenType,
  searchTokenValueSchema,
  searchTokenIdentifierSchema,
  createSearchTokenIdentifier,
  parseSearchTokenIdentifier,
  searchTokenSchema,
  validateSearchToken,
  updateReferences,
  removeReferences,
  replaceReferences,
  sortSchema,
  Sort,
  orderSchema,
  Order,
  criteriaSchema,
  validateCriteria,
} from "@shared/domains/search-token";
import {
  SearchTokenMold,
  SearchTokenIdentifierMold,
  SearchTokenValueMold,
  SearchReferenceMold,
  SearchReferenceIdentifierMold,
  SearchTokenCriteriaMold,
} from "../../support/molds/domains/search-token";
import { TimelineMold } from "../../support/molds/domains/common/date";
import { TagIdentifierMold } from "../../support/molds/domains/attributes/tag";
import {
  describeStringLengthSchema,
  describeEnumSchema,
} from "../../support/helpers";

describe("domains/search-token/common", () => {
  describe("searchTokenTypeSchema", () => {
    describeEnumSchema("SearchTokenType", searchTokenTypeSchema, ["tag", "ngram"], {
      TAG: SearchTokenType.TAG,
      NGRAM: SearchTokenType.NGRAM,
    });
  });

  describe("searchTokenValueSchema", () => {
    describeStringLengthSchema("SearchTokenValue", searchTokenValueSchema, 1, 100);

    it("Forgerで生成した値は有効", () => {
      const result = searchTokenValueSchema.safeParse(Forger(SearchTokenValueMold).forge());
      expect(result.success).toBe(true);
    });
  });

  describe("searchTokenIdentifierSchema", () => {
    describe("有効なSearchTokenIdentifierの検証", () => {
      it("'tag:value' 形式は有効", () => {
        const result = searchTokenIdentifierSchema.safeParse("tag:value");
        expect(result.success).toBe(true);
      });

      it("'ngram:value' 形式は有効", () => {
        const result = searchTokenIdentifierSchema.safeParse("ngram:value");
        expect(result.success).toBe(true);
      });

      it("コロンを含む値も有効", () => {
        const result = searchTokenIdentifierSchema.safeParse("tag:value:with:colons");
        expect(result.success).toBe(true);
      });

      it("Forgerで生成した識別子は有効", () => {
        const result = searchTokenIdentifierSchema.safeParse(Forger(SearchTokenIdentifierMold).forge());
        expect(result.success).toBe(true);
      });
    });

    describe("無効なSearchTokenIdentifierの検証", () => {
      it("コロンがない場合は無効", () => {
        const result = searchTokenIdentifierSchema.safeParse("tagvalue");
        expect(result.success).toBe(false);
      });

      it("空文字列は無効", () => {
        const result = searchTokenIdentifierSchema.safeParse("");
        expect(result.success).toBe(false);
      });

      it("無効なtypeの場合は無効", () => {
        const result = searchTokenIdentifierSchema.safeParse("invalid:value");
        expect(result.success).toBe(false);
      });
    });
  });

  describe("createSearchTokenIdentifier", () => {
    it("type=tagでidentifierを作成する", () => {
      const value = Forger(SearchTokenValueMold).forge();
      const identifier = createSearchTokenIdentifier(SearchTokenType.TAG, value);
      expect(identifier).toBe(`tag:${value}`);
    });

    it("type=ngramでidentifierを作成する", () => {
      const value = Forger(SearchTokenValueMold).forge();
      const identifier = createSearchTokenIdentifier(SearchTokenType.NGRAM, value);
      expect(identifier).toBe(`ngram:${value}`);
    });
  });

  describe("parseSearchTokenIdentifier", () => {
    it("tag:valueからtypeとvalueをパースする", () => {
      const identifier = createSearchTokenIdentifier(
        SearchTokenType.TAG,
        searchTokenValueSchema.parse("testvalue")
      );
      const parsed = parseSearchTokenIdentifier(identifier);

      expect(parsed.type).toBe(SearchTokenType.TAG);
      expect(parsed.value).toBe("testvalue");
    });

    it("ngram:valueからtypeとvalueをパースする", () => {
      const identifier = createSearchTokenIdentifier(
        SearchTokenType.NGRAM,
        searchTokenValueSchema.parse("testvalue")
      );
      const parsed = parseSearchTokenIdentifier(identifier);

      expect(parsed.type).toBe(SearchTokenType.NGRAM);
      expect(parsed.value).toBe("testvalue");
    });

    it("コロンを含む値も正しくパースする", () => {
      const identifier = searchTokenIdentifierSchema.parse("tag:value:with:colons");
      const parsed = parseSearchTokenIdentifier(identifier);

      expect(parsed.type).toBe(SearchTokenType.TAG);
      expect(parsed.value).toBe("value:with:colons");
    });
  });

  describe("searchTokenSchema", () => {
    describe("有効なSearchTokenの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = searchTokenSchema.safeParse(Forger(SearchTokenMold).forge());
        expect(result.success).toBe(true);
      });

      it("referencesが空配列でも有効", () => {
        const result = searchTokenSchema.safeParse(Forger(SearchTokenMold).forge({ references: [] }));
        expect(result.success).toBe(true);
      });
    });

    describe("無効なSearchTokenの検証", () => {
      const createTokenWithOverrides = (overrides: Record<string, unknown>) => {
        const value = Forger(SearchTokenValueMold).forge();
        return {
          identifier: `tag:${value}`,
          references: [],
          type: "tag",
          value,
          timeline: Forger(TimelineMold).forge(),
          ...overrides,
        };
      };

      it("identifierが無効な場合は無効", () => {
        const result = searchTokenSchema.safeParse(createTokenWithOverrides({ identifier: "invalid" }));
        expect(result.success).toBe(false);
      });

      it("typeが無効な場合は無効", () => {
        const result = searchTokenSchema.safeParse(createTokenWithOverrides({ type: "invalid" }));
        expect(result.success).toBe(false);
      });

      it("valueが空の場合は無効", () => {
        const result = searchTokenSchema.safeParse({
          identifier: "tag:",
          references: [],
          type: "tag",
          value: "",
          timeline: Forger(TimelineMold).forge(),
        });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateSearchToken", () => {
    it("有効なSearchTokenでokを返す", () => {
      const value = Forger(SearchTokenValueMold).forge();
      const result = validateSearchToken({
        identifier: `tag:${value}`,
        references: [],
        type: "tag",
        value,
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なSearchTokenでerrを返す", () => {
      const result = validateSearchToken({
        identifier: "invalid",
        references: [],
        type: "invalid",
        value: "",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("updateReferences", () => {
    it("トークンのreferencesを更新する", () => {
      const token = Forger(SearchTokenMold).forge({ references: [] });
      const newReferences = Forger(SearchReferenceMold).forgeMulti(3);
      const updatedToken = updateReferences(token, newReferences);

      expect(updatedToken.references).toHaveLength(3);
      expect(updatedToken.references).toEqual(newReferences);
    });

    it("timelineのupdatedAtが更新される", () => {
      const pastDate = new Date("2020-01-01T00:00:00Z");
      const token = Forger(SearchTokenMold).forge({
        timeline: { createdAt: pastDate, updatedAt: pastDate },
      });
      const newReferences = Forger(SearchReferenceMold).forgeMulti(1);
      const updatedToken = updateReferences(token, newReferences);

      expect(updatedToken.timeline.createdAt).toEqual(token.timeline.createdAt);
      expect(updatedToken.timeline.updatedAt.getTime()).toBeGreaterThan(pastDate.getTime());
    });

    it("元のトークンは変更されない（イミュータブル）", () => {
      const token = Forger(SearchTokenMold).forge({ references: [] });
      updateReferences(token, Forger(SearchReferenceMold).forgeMulti(3));
      expect(token.references).toHaveLength(0);
    });
  });

  describe("removeReferences", () => {
    it("指定したreferencesを削除する", () => {
      const references = Forger(SearchReferenceMold).forgeMulti(3);
      const token = Forger(SearchTokenMold).forge({ references });
      const referenceToRemove = references[0].identifier;
      const updatedToken = removeReferences(token, [referenceToRemove]);

      expect(updatedToken.references).toHaveLength(2);
      expect(
        updatedToken.references.some(
          (reference) =>
            reference.identifier.type === referenceToRemove.type &&
            reference.identifier.content === referenceToRemove.content
        )
      ).toBe(false);
    });

    it("存在しないreferenceを指定しても問題ない", () => {
      const references = Forger(SearchReferenceMold).forgeMulti(2);
      const token = Forger(SearchTokenMold).forge({ references });
      const nonExistentReference = Forger(SearchReferenceIdentifierMold).forge();
      const updatedToken = removeReferences(token, [nonExistentReference]);

      expect(updatedToken.references).toHaveLength(2);
    });
  });

  describe("replaceReferences", () => {
    it("既存のreferenceを新しいものに置き換える", () => {
      const references = Forger(SearchReferenceMold).forgeMulti(2);
      const token = Forger(SearchTokenMold).forge({ references });
      const updatedReference = { ...references[0], score: 999 };
      const updatedToken = replaceReferences(token, [updatedReference]);

      const replaced = updatedToken.references.find(
        (reference) =>
          reference.identifier.type === updatedReference.identifier.type &&
          reference.identifier.content === updatedReference.identifier.content
      );
      expect(replaced).toBeDefined();
      expect(replaced?.score).toBe(999);
    });

    it("新しいreferenceを追加する", () => {
      const references = Forger(SearchReferenceMold).forgeMulti(2);
      const token = Forger(SearchTokenMold).forge({ references });
      const newReference = Forger(SearchReferenceMold).forge();
      const updatedToken = replaceReferences(token, [newReference]);

      expect(updatedToken.references).toHaveLength(3);
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
          limit: null,
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
          limit: 50,
        });
        expect(result.success).toBe(true);
      });

      it("Forgerで生成したCriteriaは有効", () => {
        const result = criteriaSchema.safeParse(Forger(SearchTokenCriteriaMold).forge());
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
          limit: null,
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
          limit: null,
        });
        expect(result.success).toBe(false);
      });

      it("limitが0の場合は無効", () => {
        const result = criteriaSchema.safeParse({
          freeWord: null,
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: 0,
        });
        expect(result.success).toBe(false);
      });

      it("limitが101以上の場合は無効", () => {
        const result = criteriaSchema.safeParse({
          freeWord: null,
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: 101,
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
        limit: 10,
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
        limit: null,
      });
      expect(result.isErr).toBe(true);
    });
  });
});
