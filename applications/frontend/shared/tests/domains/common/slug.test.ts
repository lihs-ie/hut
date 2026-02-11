import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { slugSchema, validateSlug } from "@shared/domains/common/slug";
import { SlugMold } from "../../support/molds/domains/common/slug";

describe("domains/common/slug", () => {
  describe("slugSchema", () => {
    describe("有効なSlugの検証", () => {
      it("Forgerで生成したSlugは有効", () => {
        const slug = Forger(SlugMold).forge();
        const result = slugSchema.safeParse(slug);
        expect(result.success).toBe(true);
      });

      it("小文字のアルファベットのみは有効", () => {
        const result = slugSchema.safeParse("abcdefghij");
        expect(result.success).toBe(true);
      });

      it("数字のみは有効", () => {
        const result = slugSchema.safeParse("1234567890");
        expect(result.success).toBe(true);
      });

      it("ハイフン付きは有効", () => {
        const result = slugSchema.safeParse("test-slug-123");
        expect(result.success).toBe(true);
      });

      it("1文字は有効", () => {
        const result = slugSchema.safeParse("a");
        expect(result.success).toBe(true);
      });

      it("100文字は有効", () => {
        const result = slugSchema.safeParse("a".repeat(100));
        expect(result.success).toBe(true);
      });
    });

    describe("無効なSlugの検証", () => {
      it("空文字列は無効", () => {
        const result = slugSchema.safeParse("");
        expect(result.success).toBe(false);
      });

      it("101文字以上は無効", () => {
        const result = slugSchema.safeParse("a".repeat(101));
        expect(result.success).toBe(false);
      });

      it("大文字を含む場合は無効", () => {
        const result = slugSchema.safeParse("Test-Slug");
        expect(result.success).toBe(false);
      });

      it("スペースを含む場合は無効", () => {
        const result = slugSchema.safeParse("test slug");
        expect(result.success).toBe(false);
      });

      it("アンダースコアを含む場合は無効", () => {
        const result = slugSchema.safeParse("test_slug");
        expect(result.success).toBe(false);
      });

      it("日本語を含む場合は無効", () => {
        const result = slugSchema.safeParse("test-スラッグ");
        expect(result.success).toBe(false);
      });

      it("特殊文字を含む場合は無効", () => {
        const result = slugSchema.safeParse("test!@#$%");
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = slugSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = slugSchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });

      it("数値は無効", () => {
        const result = slugSchema.safeParse(12345);
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateSlug", () => {
    it("有効なSlugでokを返す", () => {
      const result = validateSlug("valid-slug-123");
      expect(result.isOk).toBe(true);
      if (result.isOk) {
        expect(result.unwrap()).toBe("valid-slug-123");
      }
    });

    it("Forgerで生成したSlugでokを返す", () => {
      const slug = Forger(SlugMold).forge();
      const result = validateSlug(slug);
      expect(result.isOk).toBe(true);
    });

    it("無効なSlugでerrを返す", () => {
      const result = validateSlug("Invalid Slug");
      expect(result.isErr).toBe(true);
      if (result.isErr) {
        expect(result.unwrapError().field).toBe("Slug");
      }
    });

    it("空文字列でerrを返す", () => {
      const result = validateSlug("");
      expect(result.isErr).toBe(true);
    });

    it("大文字を含むSlugでerrを返す", () => {
      const result = validateSlug("TEST");
      expect(result.isErr).toBe(true);
    });
  });
});
