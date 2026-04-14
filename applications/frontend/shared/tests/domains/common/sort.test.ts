import { describe, it, expect } from "vitest";
import {
  sortByFieldSchema,
  orderSchema,
  SortByField,
  Order,
} from "@shared/domains/common/sort";

describe("domains/common/sort", () => {
  describe("sortByFieldSchema", () => {
    it("createdAtを受け入れる", () => {
      const result = sortByFieldSchema.safeParse("createdAt");
      expect(result.success).toBe(true);
    });

    it("updatedAtを受け入れる", () => {
      const result = sortByFieldSchema.safeParse("updatedAt");
      expect(result.success).toBe(true);
    });

    it("不正値はパースに失敗する", () => {
      const result = sortByFieldSchema.safeParse("invalid");
      expect(result.success).toBe(false);
    });

    it("空文字列はパースに失敗する", () => {
      const result = sortByFieldSchema.safeParse("");
      expect(result.success).toBe(false);
    });
  });

  describe("orderSchema", () => {
    it("ascを受け入れる", () => {
      const result = orderSchema.safeParse("asc");
      expect(result.success).toBe(true);
    });

    it("descを受け入れる", () => {
      const result = orderSchema.safeParse("desc");
      expect(result.success).toBe(true);
    });

    it("不正値はパースに失敗する", () => {
      const result = orderSchema.safeParse("invalid");
      expect(result.success).toBe(false);
    });

    it("空文字列はパースに失敗する", () => {
      const result = orderSchema.safeParse("");
      expect(result.success).toBe(false);
    });
  });

  describe("SortByField定数", () => {
    it("CREATED_ATはcreatedAtを返す", () => {
      expect(SortByField.CREATED_AT).toBe("createdAt");
    });

    it("UPDATED_ATはupdatedAtを返す", () => {
      expect(SortByField.UPDATED_AT).toBe("updatedAt");
    });
  });

  describe("Order定数", () => {
    it("ASCはascを返す", () => {
      expect(Order.ASC).toBe("asc");
    });

    it("DESCはdescを返す", () => {
      expect(Order.DESC).toBe("desc");
    });
  });
});
