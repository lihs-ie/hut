import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  publishStatusSchema,
  PublishStatus,
} from "@shared/domains/common/status";
import { PublishStatusMold } from "../../support/molds/domains/common/status";
import { describeEnumSchema } from "../../support/helpers/schema-test";

describe("domains/common/status", () => {
  describe("publishStatusSchema", () => {
    describeEnumSchema(
      "PublishStatus",
      publishStatusSchema,
      ["draft", "published", "archived"],
      {
        DRAFT: PublishStatus.DRAFT,
        PUBLISHED: PublishStatus.PUBLISHED,
        ARCHIVED: PublishStatus.ARCHIVED,
      }
    );

    it("Forgerで生成したステータスは有効", () => {
      const status = Forger(PublishStatusMold).forge();
      const result = publishStatusSchema.safeParse(status);
      expect(result.success).toBe(true);
    });

    it("複数の異なるステータスを生成できる", () => {
      const statuses = Forger(PublishStatusMold).forgeMulti(10);
      for (const status of statuses) {
        const result = publishStatusSchema.safeParse(status);
        expect(result.success).toBe(true);
      }
    });

    it("大文字のステータスは無効", () => {
      const result = publishStatusSchema.safeParse("DRAFT");
      expect(result.success).toBe(false);
    });

    it("数値は無効", () => {
      const result = publishStatusSchema.safeParse(1);
      expect(result.success).toBe(false);
    });

    it("undefinedは無効", () => {
      const result = publishStatusSchema.safeParse(undefined);
      expect(result.success).toBe(false);
    });
  });
});
