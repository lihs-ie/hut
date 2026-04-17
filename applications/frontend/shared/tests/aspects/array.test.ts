import { describe, it, expect } from "vitest";
import { chunk, FIRESTORE_IN_BATCH_LIMIT } from "@shared/aspects/array";

describe("aspects/array", () => {
  describe("FIRESTORE_IN_BATCH_LIMIT", () => {
    it("30 である", () => {
      expect(FIRESTORE_IN_BATCH_LIMIT).toBe(30);
    });
  });

  describe("chunk", () => {
    it("空配列を渡すと空配列を返す", () => {
      const result = chunk([], 10);

      expect(result).toEqual([]);
    });

    it("items.length <= size の場合は単一チャンクを返す", () => {
      const items = [1, 2, 3, 4, 5];

      const result = chunk(items, 10);

      expect(result).toEqual([[1, 2, 3, 4, 5]]);
    });

    it("items.length === size の場合は単一チャンクを返す", () => {
      const items = [1, 2, 3];

      const result = chunk(items, 3);

      expect(result).toEqual([[1, 2, 3]]);
    });

    it("items.length > size の場合は size 毎に分割する", () => {
      const items = Array.from({ length: 100 }, (_, index) => index);

      const result = chunk(items, 30);

      expect(result.length).toBe(4);
      expect(result[0]?.length).toBe(30);
      expect(result[1]?.length).toBe(30);
      expect(result[2]?.length).toBe(30);
      expect(result[3]?.length).toBe(10);
    });

    it("size = 1 の場合は各要素が個別チャンクになる", () => {
      const items = [1, 2, 3];

      const result = chunk(items, 1);

      expect(result).toEqual([[1], [2], [3]]);
    });

    it("size が 0 以下の場合は Error をスローする", () => {
      expect(() => chunk([1, 2, 3], 0)).toThrow();
      expect(() => chunk([1, 2, 3], -1)).toThrow();
    });

    it("string 型配列を分割できる", () => {
      const items = ["a", "b", "c", "d", "e"];

      const result = chunk(items, 3);

      expect(result).toEqual([["a", "b", "c"], ["d", "e"]]);
    });
  });
});
