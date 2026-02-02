import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ExternalServiceType,
  externalServiceTypeSchema,
  externalServiceSchema,
} from "@shared/domains/common/service";
import {
  ExternalServiceTypeMold,
  ExternalServiceMold,
} from "../../support/molds/domains/common/service";
import { describeEnumSchema } from "../../support/helpers/schema-test";

describe("domains/common/service", () => {
  describe("externalServiceTypeSchema", () => {
    describeEnumSchema(
      "ExternalServiceType",
      externalServiceTypeSchema,
      ["gitHub", "x"],
      {
        GITHUB: ExternalServiceType.GITHUB,
        X: ExternalServiceType.X,
      }
    );

    it("Forgerで生成したタイプは有効", () => {
      const type = Forger(ExternalServiceTypeMold).forge();
      const result = externalServiceTypeSchema.safeParse(type);
      expect(result.success).toBe(true);
    });
  });

  describe("externalServiceSchema", () => {
    describe("有効なExternalServiceの検証", () => {
      it("Forgerで生成したExternalServiceは有効", () => {
        const service = Forger(ExternalServiceMold).forge();
        const result = externalServiceSchema.safeParse(service);
        expect(result.success).toBe(true);
      });

      it("GitHubタイプのサービスは有効", () => {
        const result = externalServiceSchema.safeParse({
          type: "gitHub",
          user: "octocat",
        });
        expect(result.success).toBe(true);
      });

      it("Xタイプのサービスは有効", () => {
        const result = externalServiceSchema.safeParse({
          type: "x",
          user: "twitteruser",
        });
        expect(result.success).toBe(true);
      });

      it("userが1文字でも有効", () => {
        const result = externalServiceSchema.safeParse({
          type: "gitHub",
          user: "a",
        });
        expect(result.success).toBe(true);
      });

      it("userが50文字でも有効", () => {
        const result = externalServiceSchema.safeParse({
          type: "gitHub",
          user: "a".repeat(50),
        });
        expect(result.success).toBe(true);
      });

      it("複数のExternalServiceを生成できる", () => {
        const services = Forger(ExternalServiceMold).forgeMulti(5);
        for (const service of services) {
          const result = externalServiceSchema.safeParse(service);
          expect(result.success).toBe(true);
        }
      });
    });

    describe("無効なExternalServiceの検証", () => {
      it("typeが無効な場合は無効", () => {
        const result = externalServiceSchema.safeParse({
          type: "facebook",
          user: "testuser",
        });
        expect(result.success).toBe(false);
      });

      it("userが空文字列の場合は無効", () => {
        const result = externalServiceSchema.safeParse({
          type: "gitHub",
          user: "",
        });
        expect(result.success).toBe(false);
      });

      it("userが51文字以上の場合は無効", () => {
        const result = externalServiceSchema.safeParse({
          type: "gitHub",
          user: "a".repeat(51),
        });
        expect(result.success).toBe(false);
      });

      it("typeが欠けている場合は無効", () => {
        const result = externalServiceSchema.safeParse({
          user: "testuser",
        });
        expect(result.success).toBe(false);
      });

      it("userが欠けている場合は無効", () => {
        const result = externalServiceSchema.safeParse({
          type: "gitHub",
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = externalServiceSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = externalServiceSchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });

      it("空のオブジェクトは無効", () => {
        const result = externalServiceSchema.safeParse({});
        expect(result.success).toBe(false);
      });
    });
  });
});
