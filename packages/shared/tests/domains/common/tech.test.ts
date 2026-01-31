import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  TechnologyCategory,
  technologyCategorySchema,
  ExperienceType,
  experienceTypeSchema,
  technologyStackSchema,
  validateTechnologyStack,
} from "@shared/domains/common/tech";
import {
  TechnologyCategoryMold,
  ExperienceTypeMold,
  TechnologyStackMold,
} from "../../support/molds/domains/common/tech";
import { TagIdentifierMold } from "../../support/molds/domains/attributes/tag";
import { describeEnumSchema } from "../../support/helpers/schema-test";

describe("domains/common/tech", () => {
  describe("technologyCategorySchema", () => {
    describeEnumSchema(
      "TechnologyCategory",
      technologyCategorySchema,
      ["frontend", "backend"],
      {
        FRONTEND: TechnologyCategory.FRONTEND,
        BACKEND: TechnologyCategory.BACKEND,
      }
    );

    it("Forgerで生成したカテゴリは有効", () => {
      const category = Forger(TechnologyCategoryMold).forge();
      const result = technologyCategorySchema.safeParse(category);
      expect(result.success).toBe(true);
    });
  });

  describe("experienceTypeSchema", () => {
    describeEnumSchema(
      "ExperienceType",
      experienceTypeSchema,
      ["personal", "business", "both"],
      {
        PERSONAL: ExperienceType.PERSONAL,
        BUSINESS: ExperienceType.BUSINESS,
        BOTH: ExperienceType.BOTH,
      }
    );

    it("Forgerで生成したタイプは有効", () => {
      const type = Forger(ExperienceTypeMold).forge();
      const result = experienceTypeSchema.safeParse(type);
      expect(result.success).toBe(true);
    });
  });

  describe("technologyStackSchema", () => {
    describe("有効なTechnologyStackの検証", () => {
      it("Forgerで生成したTechnologyStackは有効", () => {
        const stack = Forger(TechnologyStackMold).forge();
        const result = technologyStackSchema.safeParse(stack);
        expect(result.success).toBe(true);
      });

      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = technologyStackSchema.safeParse({
          tag: Forger(TagIdentifierMold).forge(),
          from: new Date("2020-01-01"),
          continue: true,
          type: "personal",
        });
        expect(result.success).toBe(true);
      });

      it("continueがfalseでも有効", () => {
        const result = technologyStackSchema.safeParse({
          tag: Forger(TagIdentifierMold).forge(),
          from: new Date("2020-01-01"),
          continue: false,
          type: "business",
        });
        expect(result.success).toBe(true);
      });

      it("複数のTechnologyStackを生成できる", () => {
        const stacks = Forger(TechnologyStackMold).forgeMulti(5);
        for (const stack of stacks) {
          const result = technologyStackSchema.safeParse(stack);
          expect(result.success).toBe(true);
        }
      });
    });

    describe("無効なTechnologyStackの検証", () => {
      it("tagが無効な場合は無効", () => {
        const result = technologyStackSchema.safeParse({
          tag: "invalid-tag",
          from: new Date("2020-01-01"),
          continue: true,
          type: "personal",
        });
        expect(result.success).toBe(false);
      });

      it("fromがDateでない場合は無効", () => {
        const result = technologyStackSchema.safeParse({
          tag: Forger(TagIdentifierMold).forge(),
          from: "2020-01-01",
          continue: true,
          type: "personal",
        });
        expect(result.success).toBe(false);
      });

      it("continueがbooleanでない場合は無効", () => {
        const result = technologyStackSchema.safeParse({
          tag: Forger(TagIdentifierMold).forge(),
          from: new Date("2020-01-01"),
          continue: "yes",
          type: "personal",
        });
        expect(result.success).toBe(false);
      });

      it("typeが無効な場合は無効", () => {
        const result = technologyStackSchema.safeParse({
          tag: Forger(TagIdentifierMold).forge(),
          from: new Date("2020-01-01"),
          continue: true,
          type: "invalid",
        });
        expect(result.success).toBe(false);
      });

      it("tagが欠けている場合は無効", () => {
        const result = technologyStackSchema.safeParse({
          from: new Date("2020-01-01"),
          continue: true,
          type: "personal",
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = technologyStackSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = technologyStackSchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateTechnologyStack", () => {
    it("有効なUnvalidatedTechnologyStackでokを返す", () => {
      const result = validateTechnologyStack({
        tag: Forger(TagIdentifierMold).forge(),
        from: new Date("2020-01-01"),
        continue: true,
        type: "personal",
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedTechnologyStackでerrを返す", () => {
      const result = validateTechnologyStack({
        tag: "invalid",
        from: new Date("2020-01-01"),
        continue: true,
        type: "invalid",
      });
      expect(result.isErr).toBe(true);
    });
  });
});
