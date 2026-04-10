import { describe, it, expect } from "vitest";
import {
  toJstDateKey,
  resolveDateRange,
  resolvePreviousDateRange,
  generateDateKeys,
  calculateTrendPercentage,
  validatePeriod,
  validatePeriodComparison,
  validateTrendPoint,
  validateRankedItem,
  validateDistribution,
  periodSchema,
  dateKeySchema,
  dateRangeSchema,
  periodComparisonSchema,
} from "@shared/domains/analytics/common";

describe("domains/analytics/common", () => {
  describe("toJstDateKey", () => {
    it("UTC 00:00 はJST 09:00 なので同日のDateKeyを返す", () => {
      const utcMidnight = new Date("2024-01-15T00:00:00Z");
      const result = toJstDateKey(utcMidnight);
      expect(result).toBe("2024-01-15");
    });

    it("UTC 15:00 はJST 翌日 00:00 なので翌日のDateKeyを返す", () => {
      const utcBeforeMidnight = new Date("2024-01-15T15:00:00Z");
      const result = toJstDateKey(utcBeforeMidnight);
      expect(result).toBe("2024-01-16");
    });

    it("UTC 23:59 はJST 翌日 08:59 なので翌日のDateKeyを返す", () => {
      const utcEndOfDay = new Date("2024-01-15T23:59:00Z");
      const result = toJstDateKey(utcEndOfDay);
      expect(result).toBe("2024-01-16");
    });

    it("UTC 14:59 はJST 23:59 なので同日のDateKeyを返す", () => {
      const utcJustBeforeJstMidnight = new Date("2024-01-15T14:59:00Z");
      const result = toJstDateKey(utcJustBeforeJstMidnight);
      expect(result).toBe("2024-01-15");
    });

    it("DateKeyはYYYY-MM-DD形式の文字列を返す", () => {
      const date = new Date("2024-03-01T00:00:00Z");
      const result = toJstDateKey(date);
      expect(result).toMatch(/^\d{4}-\d{2}-\d{2}$/);
    });

    it("dateKeySchemaでパース可能な値を返す", () => {
      const date = new Date("2024-01-01T00:00:00Z");
      const result = toJstDateKey(date);
      expect(() => dateKeySchema.parse(result)).not.toThrow();
    });
  });

  describe("resolveDateRange", () => {
    it("7dの場合は7日前から現在までの範囲を返す", () => {
      const before = Date.now();
      const range = resolveDateRange(periodSchema.parse("7d"));
      const after = Date.now();

      const expectedStartMin = before - 7 * 24 * 60 * 60 * 1000;
      const expectedStartMax = after - 7 * 24 * 60 * 60 * 1000;

      expect(range.start.getTime()).toBeGreaterThanOrEqual(expectedStartMin);
      expect(range.start.getTime()).toBeLessThanOrEqual(expectedStartMax);
      expect(range.end.getTime()).toBeGreaterThanOrEqual(before);
      expect(range.end.getTime()).toBeLessThanOrEqual(after);
    });

    it("30dの場合は30日前から現在までの範囲を返す", () => {
      const before = Date.now();
      const range = resolveDateRange(periodSchema.parse("30d"));
      const after = Date.now();

      const expectedStartMin = before - 30 * 24 * 60 * 60 * 1000;
      const expectedStartMax = after - 30 * 24 * 60 * 60 * 1000;

      expect(range.start.getTime()).toBeGreaterThanOrEqual(expectedStartMin);
      expect(range.start.getTime()).toBeLessThanOrEqual(expectedStartMax);
      expect(range.end.getTime()).toBeGreaterThanOrEqual(before);
    });

    it("90dの場合は90日前から現在までの範囲を返す", () => {
      const before = Date.now();
      const range = resolveDateRange(periodSchema.parse("90d"));
      const after = Date.now();

      const expectedStartMin = before - 90 * 24 * 60 * 60 * 1000;
      const expectedStartMax = after - 90 * 24 * 60 * 60 * 1000;

      expect(range.start.getTime()).toBeGreaterThanOrEqual(expectedStartMin);
      expect(range.start.getTime()).toBeLessThanOrEqual(expectedStartMax);
      expect(range.end.getTime()).toBeGreaterThanOrEqual(before);
    });

    it("allの場合はALL_START_DATE(2020-01-01)から現在までの範囲を返す", () => {
      const before = Date.now();
      const range = resolveDateRange(periodSchema.parse("all"));
      const after = Date.now();

      expect(range.start).toEqual(new Date("2020-01-01T00:00:00Z"));
      expect(range.end.getTime()).toBeGreaterThanOrEqual(before);
      expect(range.end.getTime()).toBeLessThanOrEqual(after);
    });

    it("返り値はdateRangeSchemaでパース可能", () => {
      const range = resolveDateRange(periodSchema.parse("7d"));
      expect(() => dateRangeSchema.parse(range)).not.toThrow();
    });

    it("startがendより前であることを確認", () => {
      const range = resolveDateRange(periodSchema.parse("30d"));
      expect(range.start.getTime()).toBeLessThan(range.end.getTime());
    });
  });

  describe("resolvePreviousDateRange", () => {
    it("7dの場合は14日前から7日前までの範囲を返す", () => {
      const before = Date.now();
      const range = resolvePreviousDateRange(periodSchema.parse("7d"));
      const after = Date.now();

      const expectedEndMin = before - 7 * 24 * 60 * 60 * 1000;
      const expectedEndMax = after - 7 * 24 * 60 * 60 * 1000;
      const expectedStartMin = before - 14 * 24 * 60 * 60 * 1000;
      const expectedStartMax = after - 14 * 24 * 60 * 60 * 1000;

      expect(range.start.getTime()).toBeGreaterThanOrEqual(expectedStartMin);
      expect(range.start.getTime()).toBeLessThanOrEqual(expectedStartMax);
      expect(range.end.getTime()).toBeGreaterThanOrEqual(expectedEndMin);
      expect(range.end.getTime()).toBeLessThanOrEqual(expectedEndMax);
    });

    it("30dの場合は60日前から30日前までの範囲を返す", () => {
      const before = Date.now();
      const range = resolvePreviousDateRange(periodSchema.parse("30d"));
      const after = Date.now();

      const expectedEndMin = before - 30 * 24 * 60 * 60 * 1000;
      const expectedEndMax = after - 30 * 24 * 60 * 60 * 1000;

      expect(range.end.getTime()).toBeGreaterThanOrEqual(expectedEndMin);
      expect(range.end.getTime()).toBeLessThanOrEqual(expectedEndMax);
    });

    it("90dの場合は180日前から90日前までの範囲を返す", () => {
      const before = Date.now();
      const range = resolvePreviousDateRange(periodSchema.parse("90d"));
      const after = Date.now();

      const expectedEndMin = before - 90 * 24 * 60 * 60 * 1000;
      const expectedEndMax = after - 90 * 24 * 60 * 60 * 1000;

      expect(range.end.getTime()).toBeGreaterThanOrEqual(expectedEndMin);
      expect(range.end.getTime()).toBeLessThanOrEqual(expectedEndMax);
    });

    it("allの場合はALL_START_DATEからALL_START_DATEまでの範囲を返す", () => {
      const range = resolvePreviousDateRange(periodSchema.parse("all"));
      const allStartDate = new Date("2020-01-01T00:00:00Z");

      expect(range.start).toEqual(allStartDate);
      expect(range.end).toEqual(allStartDate);
    });

    it("返り値はdateRangeSchemaでパース可能", () => {
      const range = resolvePreviousDateRange(periodSchema.parse("7d"));
      expect(() => dateRangeSchema.parse(range)).not.toThrow();
    });
  });

  describe("generateDateKeys", () => {
    it("1日の範囲の場合は1つのDateKeyを返す", () => {
      const range = dateRangeSchema.parse({
        start: new Date("2024-01-01T00:00:00Z"),
        end: new Date("2024-01-01T00:00:00Z"),
      });
      const keys = generateDateKeys(range);
      expect(keys).toHaveLength(1);
      expect(keys[0]).toBe("2024-01-01");
    });

    it("3日の範囲の場合は3つのDateKeyを返す", () => {
      const range = dateRangeSchema.parse({
        start: new Date("2024-01-01T00:00:00Z"),
        end: new Date("2024-01-03T00:00:00Z"),
      });
      const keys = generateDateKeys(range);
      expect(keys).toHaveLength(3);
    });

    it("7日の範囲の場合は7つのDateKeyを返す", () => {
      const range = dateRangeSchema.parse({
        start: new Date("2024-01-01T00:00:00Z"),
        end: new Date("2024-01-07T00:00:00Z"),
      });
      const keys = generateDateKeys(range);
      expect(keys).toHaveLength(7);
    });

    it("日付が昇順で並んでいる", () => {
      const range = dateRangeSchema.parse({
        start: new Date("2024-01-01T00:00:00Z"),
        end: new Date("2024-01-05T00:00:00Z"),
      });
      const keys = generateDateKeys(range);
      expect(keys[0]).toBe("2024-01-01");
      expect(keys[1]).toBe("2024-01-02");
      expect(keys[2]).toBe("2024-01-03");
      expect(keys[3]).toBe("2024-01-04");
      expect(keys[4]).toBe("2024-01-05");
    });

    it("月をまたぐ範囲でも正しくDateKeyを生成する", () => {
      const range = dateRangeSchema.parse({
        start: new Date("2024-01-30T00:00:00Z"),
        end: new Date("2024-02-02T00:00:00Z"),
      });
      const keys = generateDateKeys(range);
      expect(keys).toHaveLength(4);
      expect(keys[0]).toBe("2024-01-30");
      expect(keys[1]).toBe("2024-01-31");
      expect(keys[2]).toBe("2024-02-01");
      expect(keys[3]).toBe("2024-02-02");
    });

    it("全てのキーがdateKeySchemaでパース可能", () => {
      const range = dateRangeSchema.parse({
        start: new Date("2024-03-01T00:00:00Z"),
        end: new Date("2024-03-03T00:00:00Z"),
      });
      const keys = generateDateKeys(range);
      keys.forEach((key) => {
        expect(() => dateKeySchema.parse(key)).not.toThrow();
      });
    });
  });

  describe("calculateTrendPercentage", () => {
    it("previousが0でcurrentが正の場合は100を返す", () => {
      expect(calculateTrendPercentage(10, 0)).toBe(100);
    });

    it("previousが0でcurrentが0の場合は0を返す", () => {
      expect(calculateTrendPercentage(0, 0)).toBe(0);
    });

    it("currentとpreviousが同じ場合は0を返す", () => {
      expect(calculateTrendPercentage(100, 100)).toBe(0);
    });

    it("currentがpreviousの2倍の場合は100を返す", () => {
      expect(calculateTrendPercentage(200, 100)).toBe(100);
    });

    it("currentがpreviousの半分の場合は-50を返す", () => {
      expect(calculateTrendPercentage(50, 100)).toBe(-50);
    });

    it("currentがpreviousより増加した場合は正の値を返す", () => {
      const result = calculateTrendPercentage(150, 100);
      expect(result).toBe(50);
    });

    it("currentがpreviousより減少した場合は負の値を返す", () => {
      const result = calculateTrendPercentage(80, 100);
      expect(result).toBe(-20);
    });

    it("結果はMath.roundで丸められる", () => {
      const result = calculateTrendPercentage(10, 3);
      expect(Number.isInteger(result)).toBe(true);
      expect(result).toBe(Math.round(((10 - 3) / 3) * 100));
    });

    it("previousが0でcurrentが0より小さい場合は0を返す", () => {
      expect(calculateTrendPercentage(-1, 0)).toBe(0);
    });
  });

  describe("validatePeriod", () => {
    it("7dはokを返す", () => {
      const result = validatePeriod("7d");
      expect(result.isOk).toBe(true);
    });

    it("30dはokを返す", () => {
      const result = validatePeriod("30d");
      expect(result.isOk).toBe(true);
    });

    it("90dはokを返す", () => {
      const result = validatePeriod("90d");
      expect(result.isOk).toBe(true);
    });

    it("allはokを返す", () => {
      const result = validatePeriod("all");
      expect(result.isOk).toBe(true);
    });

    it("不正な文字列はerrを返す", () => {
      const result = validatePeriod("invalid");
      expect(result.isErr).toBe(true);
    });

    it("空文字列はerrを返す", () => {
      const result = validatePeriod("");
      expect(result.isErr).toBe(true);
    });

    it("1dはerrを返す", () => {
      const result = validatePeriod("1d");
      expect(result.isErr).toBe(true);
    });

    it("okの場合はperiodSchemaでパース可能な値を含む", () => {
      const result = validatePeriod("7d");
      expect(result.isOk).toBe(true);
      const period = result.unwrap();
      expect(() => periodSchema.parse(period)).not.toThrow();
    });

    it("errの場合はValidationErrorを含む", () => {
      const result = validatePeriod("bad");
      expect(result.isErr).toBe(true);
      const error = result.unwrapError();
      expect(error).toHaveProperty("field");
      expect(error).toHaveProperty("description");
    });
  });

  describe("validatePeriodComparison", () => {
    it("有効なPeriodComparisonでokを返す", () => {
      const result = validatePeriodComparison({ current: 100, previous: 80 });
      expect(result.isOk).toBe(true);
    });

    it("currentとpreviousが0でもokを返す", () => {
      const result = validatePeriodComparison({ current: 0, previous: 0 });
      expect(result.isOk).toBe(true);
    });

    it("currentが欠けている場合はerrを返す", () => {
      const parsed = periodComparisonSchema.safeParse({ previous: 80 });
      expect(parsed.success).toBe(false);
    });

    it("previousが欠けている場合はerrを返す", () => {
      const parsed = periodComparisonSchema.safeParse({ current: 100 });
      expect(parsed.success).toBe(false);
    });
  });

  describe("validateTrendPoint", () => {
    it("有効なTrendPointでokを返す", () => {
      const result = validateTrendPoint({ dateKey: "2024-01-15", value: 42 });
      expect(result.isOk).toBe(true);
    });

    it("valueが0でもokを返す", () => {
      const result = validateTrendPoint({ dateKey: "2024-01-15", value: 0 });
      expect(result.isOk).toBe(true);
    });

    it("不正なdateKeyの場合はerrを返す", () => {
      const result = validateTrendPoint({ dateKey: "invalid-date", value: 10 });
      expect(result.isErr).toBe(true);
    });

    it("dateKeyが空文字の場合はerrを返す", () => {
      const result = validateTrendPoint({ dateKey: "", value: 10 });
      expect(result.isErr).toBe(true);
    });
  });

  describe("validateRankedItem", () => {
    it("有効なRankedItemでokを返す", () => {
      const result = validateRankedItem({ label: "TypeScript", value: 150 });
      expect(result.isOk).toBe(true);
    });

    it("subLabelありの有効なRankedItemでokを返す", () => {
      const result = validateRankedItem({
        label: "TypeScript",
        value: 150,
        subLabel: "人気言語",
      });
      expect(result.isOk).toBe(true);
    });

    it("subLabelなしでもokを返す", () => {
      const result = validateRankedItem({ label: "TypeScript", value: 150 });
      expect(result.isOk).toBe(true);
    });

    it("labelが空文字でもokを返す", () => {
      const result = validateRankedItem({ label: "", value: 0 });
      expect(result.isOk).toBe(true);
    });
  });

  describe("validateDistribution", () => {
    it("有効なDistributionでokを返す", () => {
      const result = validateDistribution({ label: "desktop", value: 60 });
      expect(result.isOk).toBe(true);
    });

    it("valueが0でもokを返す", () => {
      const result = validateDistribution({ label: "mobile", value: 0 });
      expect(result.isOk).toBe(true);
    });

    it("labelが空文字でもokを返す", () => {
      const result = validateDistribution({ label: "", value: 0 });
      expect(result.isOk).toBe(true);
    });
  });
});
