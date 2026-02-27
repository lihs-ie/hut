import { describe, it, expect } from "vitest";
import {
  Severity,
  severitySchema,
  spellCheckIssueSchema,
  spellCheckResultSchema,
} from "@shared/domains/spellcheck/common";

describe("domains/spellcheck/common", () => {
  describe("Severity", () => {
    it("定数値が正しい", () => {
      expect(Severity.ERROR).toBe("error");
      expect(Severity.WARNING).toBe("warning");
      expect(Severity.INFO).toBe("info");
    });
  });

  describe("severitySchema", () => {
    it("有効な値をパースできる", () => {
      expect(severitySchema.parse("error")).toBe("error");
      expect(severitySchema.parse("warning")).toBe("warning");
      expect(severitySchema.parse("info")).toBe("info");
    });

    it("無効な値でエラーになる", () => {
      expect(() => severitySchema.parse("invalid")).toThrow();
      expect(() => severitySchema.parse("")).toThrow();
      expect(() => severitySchema.parse(123)).toThrow();
    });
  });

  describe("spellCheckIssueSchema", () => {
    it("有効なSpellCheckIssueをパースできる", () => {
      const valid = {
        offset: 0,
        length: 5,
        word: "tset",
        suggestions: ["test", "set"],
        severity: "error",
        message: "Possible spelling mistake",
      };
      const result = spellCheckIssueSchema.safeParse(valid);
      expect(result.success).toBe(true);
    });

    it("offset=0を受け入れる", () => {
      const valid = {
        offset: 0,
        length: 1,
        word: "a",
        suggestions: [],
        severity: "warning",
        message: "test",
      };
      const result = spellCheckIssueSchema.safeParse(valid);
      expect(result.success).toBe(true);
    });

    it("offsetが負の値でエラーになる", () => {
      const invalid = {
        offset: -1,
        length: 5,
        word: "test",
        suggestions: [],
        severity: "error",
        message: "test",
      };
      const result = spellCheckIssueSchema.safeParse(invalid);
      expect(result.success).toBe(false);
    });

    it("lengthが0でエラーになる", () => {
      const invalid = {
        offset: 0,
        length: 0,
        word: "test",
        suggestions: [],
        severity: "error",
        message: "test",
      };
      const result = spellCheckIssueSchema.safeParse(invalid);
      expect(result.success).toBe(false);
    });

    it("wordが空文字でエラーになる", () => {
      const invalid = {
        offset: 0,
        length: 5,
        word: "",
        suggestions: [],
        severity: "error",
        message: "test",
      };
      const result = spellCheckIssueSchema.safeParse(invalid);
      expect(result.success).toBe(false);
    });

    it("suggestionsが空配列でも有効", () => {
      const valid = {
        offset: 10,
        length: 3,
        word: "abc",
        suggestions: [],
        severity: "info",
        message: "test",
      };
      const result = spellCheckIssueSchema.safeParse(valid);
      expect(result.success).toBe(true);
    });
  });

  describe("spellCheckResultSchema", () => {
    it("有効なSpellCheckResultをパースできる", () => {
      const valid = {
        text: "Hello wrold",
        issues: [
          {
            offset: 6,
            length: 5,
            word: "wrold",
            suggestions: ["world"],
            severity: "error",
            message: "Possible spelling mistake",
          },
        ],
        timestamp: Date.now(),
      };
      const result = spellCheckResultSchema.safeParse(valid);
      expect(result.success).toBe(true);
    });

    it("issuesが空配列でも有効", () => {
      const valid = {
        text: "Hello world",
        issues: [],
        timestamp: Date.now(),
      };
      const result = spellCheckResultSchema.safeParse(valid);
      expect(result.success).toBe(true);
    });

    it("timestampが欠けているとエラーになる", () => {
      const invalid = {
        text: "test",
        issues: [],
      };
      const result = spellCheckResultSchema.safeParse(invalid);
      expect(result.success).toBe(false);
    });
  });
});
