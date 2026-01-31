import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  tagIdentifierSchema,
  tagNameSchema,
  tagSchema,
  validateTag,
  validateTagIdentifier,
  validateTagIdentifiers,
  validateTagName,
  validateTagNames,
  criteriaSchema,
  validateCriteria,
} from "@shared/domains/attributes/tag";
import {
  TagMold,
  TagIdentifierMold,
  TagNameMold,
} from "../../support/molds/domains/attributes/tag";
import { TimelineMold } from "../../support/molds/domains/common/date";
import { ImageMold } from "../../support/molds/domains/common/image";
import {
  describeIdentifierSchema,
  describeStringLengthSchema,
} from "../../support/helpers";

describe("domains/attributes/tag", () => {
  describe("tagIdentifierSchema", () => {
    describeIdentifierSchema(
      "TagIdentifier",
      tagIdentifierSchema,
      () => Forger(TagIdentifierMold).forge(),
      (count) => Forger(TagIdentifierMold).forgeMulti(count)
    );
  });

  describe("validateTagIdentifier", () => {
    it("有効なULIDでokを返す", () => {
      const result = validateTagIdentifier(Forger(TagIdentifierMold).forge());
      expect(result.isOk).toBe(true);
    });

    it("無効な文字列でerrを返す", () => {
      const result = validateTagIdentifier("invalid");
      expect(result.isErr).toBe(true);
    });
  });

  describe("validateTagIdentifiers", () => {
    it("有効なULID配列でokを返す", () => {
      const identifiers = Forger(TagIdentifierMold).forgeMulti(3);
      const result = validateTagIdentifiers(identifiers);
      expect(result.isOk).toBe(true);
      if (result.isOk) {
        expect(result.unwrap()).toHaveLength(3);
      }
    });

    it("空配列でもokを返す", () => {
      const result = validateTagIdentifiers([]);
      expect(result.isOk).toBe(true);
      if (result.isOk) {
        expect(result.unwrap()).toHaveLength(0);
      }
    });

    it("1つでも無効なULIDがあればerrを返す", () => {
      const validIdentifier = Forger(TagIdentifierMold).forge();
      const result = validateTagIdentifiers([validIdentifier, "invalid"]);
      expect(result.isErr).toBe(true);
    });

    it("複数の無効なULIDがある場合、全てのエラーを返す", () => {
      const result = validateTagIdentifiers(["invalid1", "invalid2"]);
      expect(result.isErr).toBe(true);
      if (result.isErr) {
        expect(result.unwrapError()).toHaveLength(2);
      }
    });
  });

  describe("tagNameSchema", () => {
    describeStringLengthSchema("タグ名", tagNameSchema, 1, 20);

    it("日本語を含むタグ名は有効", () => {
      const result = tagNameSchema.safeParse("タグ名");
      expect(result.success).toBe(true);
    });
  });

  describe("validateTagName", () => {
    it("有効なタグ名でokを返す", () => {
      const result = validateTagName("ValidTag");
      expect(result.isOk).toBe(true);
    });

    it("無効なタグ名でerrを返す", () => {
      const result = validateTagName("");
      expect(result.isErr).toBe(true);
    });
  });

  describe("validateTagNames", () => {
    it("有効なタグ名配列でokを返す", () => {
      const result = validateTagNames(["Tag1", "Tag2", "Tag3"]);
      expect(result.isOk).toBe(true);
      if (result.isOk) {
        expect(result.unwrap()).toHaveLength(3);
      }
    });

    it("空配列でもokを返す", () => {
      const result = validateTagNames([]);
      expect(result.isOk).toBe(true);
    });

    it("1つでも無効なタグ名があればerrを返す", () => {
      const result = validateTagNames(["ValidTag", ""]);
      expect(result.isErr).toBe(true);
    });
  });

  describe("tagSchema", () => {
    describe("有効なTagの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = tagSchema.safeParse(Forger(TagMold).forge());
        expect(result.success).toBe(true);
      });
    });

    describe("無効なTagの検証", () => {
      const createTagWithOverrides = (overrides: Record<string, unknown>) => ({
        identifier: Forger(TagIdentifierMold).forge(),
        name: Forger(TagNameMold).forge(),
        logo: Forger(ImageMold).forge(),
        timeline: Forger(TimelineMold).forge(),
        ...overrides,
      });

      it("identifierが無効な場合は無効", () => {
        const result = tagSchema.safeParse(createTagWithOverrides({ identifier: "invalid" }));
        expect(result.success).toBe(false);
      });

      it("nameが空の場合は無効", () => {
        const result = tagSchema.safeParse(createTagWithOverrides({ name: "" }));
        expect(result.success).toBe(false);
      });

      it("nameが21文字以上の場合は無効", () => {
        const result = tagSchema.safeParse(createTagWithOverrides({ name: "a".repeat(21) }));
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateTag", () => {
    it("有効なUnvalidatedTagでokを返す", () => {
      const result = validateTag({
        identifier: Forger(TagIdentifierMold).forge(),
        name: "TestTag",
        logo: Forger(ImageMold).forge(),
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedTagでerrを返す", () => {
      const result = validateTag({
        identifier: "invalid",
        name: "",
        logo: "not-a-url",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("nameがnullで有効", () => {
        const result = criteriaSchema.safeParse({ name: null });
        expect(result.success).toBe(true);
      });

      it("nameを指定しても有効", () => {
        const result = criteriaSchema.safeParse({ name: "TestTag" });
        expect(result.success).toBe(true);
      });

      it("空文字列も有効", () => {
        const result = criteriaSchema.safeParse({ name: "" });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("nameが21文字以上の場合は無効", () => {
        const result = criteriaSchema.safeParse({ name: "a".repeat(21) });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateCriteria", () => {
    it("有効なCriteriaでokを返す", () => {
      const result = validateCriteria({ name: "TestTag" });
      expect(result.isOk).toBe(true);
    });

    it("nullでもokを返す", () => {
      const result = validateCriteria({ name: null });
      expect(result.isOk).toBe(true);
    });

    it("無効なCriteriaでerrを返す", () => {
      const result = validateCriteria({ name: "a".repeat(21) });
      expect(result.isErr).toBe(true);
    });
  });
});
