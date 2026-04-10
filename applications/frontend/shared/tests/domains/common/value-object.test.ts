import { describe, it, expect } from "vitest";
import { hash, equals } from "@shared/domains/common/value-object";

describe("domains/common/value-object", () => {
  describe("hash", () => {
    it("文字列をハッシュできる", () => {
      const result = hash("hello");
      expect(result).toBeDefined();
    });

    it("オブジェクトをハッシュできる", () => {
      const result = hash({ key: "value", count: 1 });
      expect(result).toBeDefined();
    });

    it("同じ値は同じハッシュを返す", () => {
      const first = hash("same-value");
      const second = hash("same-value");
      expect(first).toBe(second);
    });

    it("異なる値は異なるハッシュを返す", () => {
      const first = hash("value-a");
      const second = hash("value-b");
      expect(first).not.toBe(second);
    });

    it("戻り値はstring型", () => {
      const result = hash(42);
      expect(typeof result).toBe("string");
    });
  });

  describe("equals", () => {
    it("同じ文字列はtrueを返す", () => {
      expect(equals("hello", "hello")).toBe(true);
    });

    it("異なる文字列はfalseを返す", () => {
      expect(equals("hello", "world")).toBe(false);
    });

    it("同じオブジェクトはtrueを返す", () => {
      const left = { name: "test", value: 100 };
      const right = { name: "test", value: 100 };
      expect(equals(left, right)).toBe(true);
    });

    it("異なるオブジェクトはfalseを返す", () => {
      const left = { name: "test", value: 100 };
      const right = { name: "test", value: 200 };
      expect(equals(left, right)).toBe(false);
    });

    it("同じ数値はtrueを返す", () => {
      expect(equals(42, 42)).toBe(true);
    });

    it("異なる数値はfalseを返す", () => {
      expect(equals(42, 43)).toBe(false);
    });
  });
});
