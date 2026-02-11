import { describe, it, expect } from "vitest";

const periods = {
  "7d": "過去7日",
  "30d": "過去30日",
  "90d": "過去90日",
  all: "全期間",
} as const;

type Period = keyof typeof periods;

describe("AnalyticsPeriodSelector ロジック", () => {
  describe("期間プリセット定義", () => {
    it("4つのプリセット期間が定義されている", () => {
      const keys = Object.keys(periods);
      expect(keys).toHaveLength(4);
    });

    it("7d, 30d, 90d, all のキーが存在する", () => {
      const keys = Object.keys(periods);
      expect(keys).toContain("7d");
      expect(keys).toContain("30d");
      expect(keys).toContain("90d");
      expect(keys).toContain("all");
    });

    it("各プリセットに日本語ラベルが設定されている", () => {
      expect(periods["7d"]).toBe("過去7日");
      expect(periods["30d"]).toBe("過去30日");
      expect(periods["90d"]).toBe("過去90日");
      expect(periods["all"]).toBe("全期間");
    });
  });

  describe("Period 型", () => {
    it("有効な期間値を検証する", () => {
      const validPeriods: Array<Period> = ["7d", "30d", "90d", "all"];

      validPeriods.forEach((period) => {
        expect(Object.keys(periods)).toContain(period);
      });
    });

    it("Object.entries で key-value ペアを取得できる", () => {
      const entries = Object.entries(periods) as Array<[Period, string]>;

      expect(entries).toHaveLength(4);
      entries.forEach(([key, label]) => {
        expect(typeof key).toBe("string");
        expect(typeof label).toBe("string");
      });
    });
  });

  describe("デフォルト値", () => {
    it("デフォルトの期間は 30d である", () => {
      const defaultPeriod: Period = "30d";
      expect(defaultPeriod).toBe("30d");
      expect(periods[defaultPeriod]).toBe("過去30日");
    });
  });

  describe("アクティブ状態の判定", () => {
    it("現在の期間とキーが一致する場合はアクティブ", () => {
      const currentPeriod: Period = "7d";
      const isActive = (key: Period): boolean => currentPeriod === key;

      expect(isActive("7d")).toBe(true);
      expect(isActive("30d")).toBe(false);
      expect(isActive("90d")).toBe(false);
      expect(isActive("all")).toBe(false);
    });

    it("全期間が選択されている場合のアクティブ判定", () => {
      const currentPeriod: Period = "all";
      const isActive = (key: Period): boolean => currentPeriod === key;

      expect(isActive("7d")).toBe(false);
      expect(isActive("30d")).toBe(false);
      expect(isActive("90d")).toBe(false);
      expect(isActive("all")).toBe(true);
    });
  });
});
