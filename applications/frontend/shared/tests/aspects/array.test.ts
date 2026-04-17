import { describe, it, expect } from "vitest";
import { chunk } from "@shared/aspects/array";

describe("aspects/array", () => {
  describe("chunk", () => {
    it("空配列を渡すと空配列を返す", () => {
      const result = chunk([], 10);

      expect(result.isOk).toBe(true);
      expect(result.unwrap()).toEqual([]);
    });

    it("items.length <= size の場合は単一チャンクを返す", () => {
      const items = [1, 2, 3, 4, 5];

      const result = chunk(items, 10);

      expect(result.unwrap()).toEqual([[1, 2, 3, 4, 5]]);
    });

    it("items.length === size の場合は単一チャンクを返す", () => {
      const items = [1, 2, 3];

      const result = chunk(items, 3);

      expect(result.unwrap()).toEqual([[1, 2, 3]]);
    });

    it("items.length > size の場合は size 毎に分割する", () => {
      const items = Array.from({ length: 100 }, (_, index) => index);

      const result = chunk(items, 30);

      const chunks = result.unwrap();
      expect(chunks.length).toBe(4);
      expect(chunks[0]?.length).toBe(30);
      expect(chunks[1]?.length).toBe(30);
      expect(chunks[2]?.length).toBe(30);
      expect(chunks[3]?.length).toBe(10);
    });

    it("size = 1 の場合は各要素が個別チャンクになる", () => {
      const items = [1, 2, 3];

      const result = chunk(items, 1);

      expect(result.unwrap()).toEqual([[1], [2], [3]]);
    });

    it("size が 0 以下の場合は Err を返す", () => {
      const zeroResult = chunk([1, 2, 3], 0);
      expect(zeroResult.isErr).toBe(true);
      expect(zeroResult.unwrapError()).toBeInstanceOf(Error);

      const negativeResult = chunk([1, 2, 3], -1);
      expect(negativeResult.isErr).toBe(true);
      expect(negativeResult.unwrapError()).toBeInstanceOf(Error);
    });

    it("string 型配列を分割できる", () => {
      const items = ["a", "b", "c", "d", "e"];

      const result = chunk(items, 3);

      expect(result.unwrap()).toEqual([["a", "b", "c"], ["d", "e"]]);
    });
  });
});
