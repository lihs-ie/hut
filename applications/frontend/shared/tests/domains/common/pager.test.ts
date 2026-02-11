import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  pagerSchema,
  offset,
  first,
  last,
} from "@shared/domains/common/pager";
import { PagerMold } from "../../support/molds/domains/common/pager";

describe("domains/common/pager", () => {
  describe("pagerSchema", () => {
    describe("有効なPagerの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const pager = Forger(PagerMold).forge();
        const result = pagerSchema.safeParse(pager);
        expect(result.success).toBe(true);
      });

      it("totalが0でも有効", () => {
        const result = pagerSchema.safeParse({
          total: 0,
          items: 10,
          current: 0,
        });
        expect(result.success).toBe(true);
      });

      it("currentが0でも有効", () => {
        const result = pagerSchema.safeParse({
          total: 100,
          items: 10,
          current: 0,
        });
        expect(result.success).toBe(true);
      });

      it("itemsが1でも有効", () => {
        const result = pagerSchema.safeParse({
          total: 100,
          items: 1,
          current: 1,
        });
        expect(result.success).toBe(true);
      });

      it("大きな数値でも有効", () => {
        const result = pagerSchema.safeParse({
          total: 1000000,
          items: 100,
          current: 10000,
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なPagerの検証", () => {
      it("totalが負の数の場合は無効", () => {
        const result = pagerSchema.safeParse({
          total: -1,
          items: 10,
          current: 1,
        });
        expect(result.success).toBe(false);
      });

      it("itemsが負の数の場合は無効", () => {
        const result = pagerSchema.safeParse({
          total: 100,
          items: -1,
          current: 1,
        });
        expect(result.success).toBe(false);
      });

      it("currentが負の数の場合は無効", () => {
        const result = pagerSchema.safeParse({
          total: 100,
          items: 10,
          current: -1,
        });
        expect(result.success).toBe(false);
      });

      it("totalが欠けている場合は無効", () => {
        const result = pagerSchema.safeParse({
          items: 10,
          current: 1,
        });
        expect(result.success).toBe(false);
      });

      it("itemsが欠けている場合は無効", () => {
        const result = pagerSchema.safeParse({
          total: 100,
          current: 1,
        });
        expect(result.success).toBe(false);
      });

      it("currentが欠けている場合は無効", () => {
        const result = pagerSchema.safeParse({
          total: 100,
          items: 10,
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = pagerSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = pagerSchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });

      it("空のオブジェクトは無効", () => {
        const result = pagerSchema.safeParse({});
        expect(result.success).toBe(false);
      });

      it("文字列のフィールドは無効", () => {
        const result = pagerSchema.safeParse({
          total: "100",
          items: "10",
          current: "1",
        });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("offset", () => {
    it("1ページ目の場合は0を返す", () => {
      const pager = pagerSchema.parse({
        total: 100,
        items: 10,
        current: 1,
      });
      expect(offset(pager)).toBe(0);
    });

    it("2ページ目の場合はitemsを返す", () => {
      const pager = pagerSchema.parse({
        total: 100,
        items: 10,
        current: 2,
      });
      expect(offset(pager)).toBe(10);
    });

    it("10ページ目の場合は正しいオフセットを返す", () => {
      const pager = pagerSchema.parse({
        total: 100,
        items: 10,
        current: 10,
      });
      expect(offset(pager)).toBe(90);
    });

    it("currentが0の場合は負のオフセットを返す", () => {
      const pager = pagerSchema.parse({
        total: 100,
        items: 10,
        current: 0,
      });
      expect(offset(pager)).toBe(-10);
    });
  });

  describe("first", () => {
    it("totalが0より大きい場合は1を返す", () => {
      const pager = pagerSchema.parse({
        total: 100,
        items: 10,
        current: 5,
      });
      expect(first(pager)).toBe(1);
    });

    it("totalが1の場合も1を返す", () => {
      const pager = pagerSchema.parse({
        total: 1,
        items: 10,
        current: 1,
      });
      expect(first(pager)).toBe(1);
    });

    it("totalが0の場合は0を返す", () => {
      const pager = pagerSchema.parse({
        total: 0,
        items: 10,
        current: 0,
      });
      expect(first(pager)).toBe(0);
    });
  });

  describe("last", () => {
    it("total: 100, items: 10の場合は10を返す", () => {
      const pager = pagerSchema.parse({
        total: 100,
        items: 10,
        current: 1,
      });
      expect(last(pager)).toBe(10);
    });

    it("total: 101, items: 10の場合は11を返す", () => {
      const pager = pagerSchema.parse({
        total: 101,
        items: 10,
        current: 1,
      });
      expect(last(pager)).toBe(11);
    });

    it("total: 1, items: 10の場合は1を返す", () => {
      const pager = pagerSchema.parse({
        total: 1,
        items: 10,
        current: 1,
      });
      expect(last(pager)).toBe(1);
    });

    it("total: 0, items: 10の場合は0を返す", () => {
      const pager = pagerSchema.parse({
        total: 0,
        items: 10,
        current: 0,
      });
      expect(last(pager)).toBe(0);
    });

    it("total: 99, items: 10の場合は10を返す", () => {
      const pager = pagerSchema.parse({
        total: 99,
        items: 10,
        current: 1,
      });
      expect(last(pager)).toBe(10);
    });
  });
});
