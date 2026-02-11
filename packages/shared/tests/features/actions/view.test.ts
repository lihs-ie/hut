import { describe, it, expect } from "vitest";
import { getJstDateKey } from "@shared/aspects/date";

describe("Feature: View Action", () => {
  describe("JST日付キーの生成", () => {
    it("UTC時刻からJST日付キーが正しく生成される", () => {
      const utcMidnight = new Date("2024-01-15T00:00:00Z");
      expect(getJstDateKey(utcMidnight)).toBe("2024-01-15");

      const jstMidnight = new Date("2024-01-14T15:00:00Z");
      expect(getJstDateKey(jstMidnight)).toBe("2024-01-15");

      const beforeJstMidnight = new Date("2024-01-14T14:59:59Z");
      expect(getJstDateKey(beforeJstMidnight)).toBe("2024-01-14");
    });
  });
});
