import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  dwellTimeSchema,
  scrollDepthSchema,
  engagementRecordIdentifierSchema,
  engagementRecordSchema,
  equalEngagementRecordIdentifier,
  criteriaSchema,
  formatDwellTime,
  validateEngagementRecord,
  validateCriteria,
} from "@shared/domains/analytics/engagement";
import {
  EngagementRecordMold,
  EngagementRecordIdentifierMold,
  DwellTimeMold,
  ScrollDepthMold,
} from "../../support/molds/domains/analytics/engagement";
import { SearchReferenceIdentifierMold } from "../../support/molds/domains/search-token/common";

describe("domains/analytics/engagement", () => {
  // ---------------------------------------------------------------------------
  // DwellTime Value Object
  // ---------------------------------------------------------------------------
  describe("dwellTimeSchema", () => {
    describe("有効なDwellTimeの検証", () => {
      it("0秒は有効", () => {
        const result = dwellTimeSchema.safeParse(0);
        expect(result.success).toBe(true);
      });

      it("86400秒（24時間）は有効", () => {
        const result = dwellTimeSchema.safeParse(86_400);
        expect(result.success).toBe(true);
      });

      it("中間値は有効", () => {
        const result = dwellTimeSchema.safeParse(3600);
        expect(result.success).toBe(true);
      });

      it("Forgerで生成した値は有効", () => {
        const value = Forger(DwellTimeMold).forge();
        const result = dwellTimeSchema.safeParse(value);
        expect(result.success).toBe(true);
      });
    });

    describe("無効なDwellTimeの検証", () => {
      it("負の数は無効", () => {
        const result = dwellTimeSchema.safeParse(-1);
        expect(result.success).toBe(false);
      });

      it("86401秒以上は無効", () => {
        const result = dwellTimeSchema.safeParse(86_401);
        expect(result.success).toBe(false);
      });

      it("小数は無効", () => {
        const result = dwellTimeSchema.safeParse(1.5);
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = dwellTimeSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("文字列は無効", () => {
        const result = dwellTimeSchema.safeParse("100");
        expect(result.success).toBe(false);
      });
    });
  });

  // ---------------------------------------------------------------------------
  // ScrollDepth Value Object
  // ---------------------------------------------------------------------------
  describe("scrollDepthSchema", () => {
    describe("有効なScrollDepthの検証", () => {
      it("0%は有効", () => {
        const result = scrollDepthSchema.safeParse(0);
        expect(result.success).toBe(true);
      });

      it("100%は有効", () => {
        const result = scrollDepthSchema.safeParse(100);
        expect(result.success).toBe(true);
      });

      it("50%は有効", () => {
        const result = scrollDepthSchema.safeParse(50);
        expect(result.success).toBe(true);
      });

      it("Forgerで生成した値は有効", () => {
        const value = Forger(ScrollDepthMold).forge();
        const result = scrollDepthSchema.safeParse(value);
        expect(result.success).toBe(true);
      });
    });

    describe("無効なScrollDepthの検証", () => {
      it("負の数は無効", () => {
        const result = scrollDepthSchema.safeParse(-1);
        expect(result.success).toBe(false);
      });

      it("101以上は無効", () => {
        const result = scrollDepthSchema.safeParse(101);
        expect(result.success).toBe(false);
      });

      it("小数は無効", () => {
        const result = scrollDepthSchema.safeParse(50.5);
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = scrollDepthSchema.safeParse(null);
        expect(result.success).toBe(false);
      });
    });
  });

  // ---------------------------------------------------------------------------
  // EngagementRecordIdentifier
  // ---------------------------------------------------------------------------
  describe("engagementRecordIdentifierSchema", () => {
    describe("有効なEngagementRecordIdentifierの検証", () => {
      it("Forgerで生成した値は有効", () => {
        const identifier = Forger(EngagementRecordIdentifierMold).forge();
        const result = engagementRecordIdentifierSchema.safeParse(identifier);
        expect(result.success).toBe(true);
      });

      it("複数生成した値は全て有効", () => {
        const identifiers =
          Forger(EngagementRecordIdentifierMold).forgeMulti(5);
        for (const identifier of identifiers) {
          const result = engagementRecordIdentifierSchema.safeParse(identifier);
          expect(result.success).toBe(true);
        }
      });
    });

    describe("無効なEngagementRecordIdentifierの検証", () => {
      it("referenceが欠けている場合は無効", () => {
        const result = engagementRecordIdentifierSchema.safeParse({
          dateKey: "2024-01-01",
          sessionKey: "550e8400-e29b-41d4-a716-446655440000",
        });
        expect(result.success).toBe(false);
      });

      it("dateKeyが不正な形式の場合は無効", () => {
        const reference = Forger(SearchReferenceIdentifierMold).forge();
        const result = engagementRecordIdentifierSchema.safeParse({
          reference,
          dateKey: "20240101",
          sessionKey: "550e8400-e29b-41d4-a716-446655440000",
        });
        expect(result.success).toBe(false);
      });

      it("sessionKeyが不正な形式の場合は無効", () => {
        const reference = Forger(SearchReferenceIdentifierMold).forge();
        const result = engagementRecordIdentifierSchema.safeParse({
          reference,
          dateKey: "2024-01-01",
          sessionKey: "not-a-uuid",
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = engagementRecordIdentifierSchema.safeParse(null);
        expect(result.success).toBe(false);
      });
    });
  });

  // ---------------------------------------------------------------------------
  // equalEngagementRecordIdentifier
  // ---------------------------------------------------------------------------
  describe("equalEngagementRecordIdentifier", () => {
    it("同一の識別子は等しい", () => {
      const identifier = Forger(EngagementRecordIdentifierMold).forge();
      expect(equalEngagementRecordIdentifier(identifier, identifier)).toBe(
        true,
      );
    });

    it("同じ値を持つ異なるオブジェクトは等しい", () => {
      const identifier = Forger(EngagementRecordIdentifierMold).forge();
      const clone = engagementRecordIdentifierSchema.parse({
        reference: { ...identifier.reference },
        dateKey: identifier.dateKey,
        sessionKey: identifier.sessionKey,
      });
      expect(equalEngagementRecordIdentifier(identifier, clone)).toBe(true);
    });

    it("異なる識別子は等しくない", () => {
      const identifiers =
        Forger(EngagementRecordIdentifierMold).forgeMulti(2);
      expect(
        equalEngagementRecordIdentifier(identifiers[0], identifiers[1]),
      ).toBe(false);
    });

    it("dateKeyが異なる場合は等しくない", () => {
      const identifier = Forger(EngagementRecordIdentifierMold).forge();
      const different = engagementRecordIdentifierSchema.parse({
        reference: identifier.reference,
        dateKey: "2099-12-31",
        sessionKey: identifier.sessionKey,
      });
      expect(equalEngagementRecordIdentifier(identifier, different)).toBe(
        false,
      );
    });

    it("sessionKeyが異なる場合は等しくない", () => {
      const identifier = Forger(EngagementRecordIdentifierMold).forge();
      const different = engagementRecordIdentifierSchema.parse({
        reference: identifier.reference,
        dateKey: identifier.dateKey,
        sessionKey: "00000000-0000-4000-a000-000000000000",
      });
      expect(equalEngagementRecordIdentifier(identifier, different)).toBe(
        false,
      );
    });
  });

  // ---------------------------------------------------------------------------
  // EngagementRecord Aggregate
  // ---------------------------------------------------------------------------
  describe("engagementRecordSchema", () => {
    describe("有効なEngagementRecordの検証", () => {
      it("Forgerで生成した値は有効", () => {
        const record = Forger(EngagementRecordMold).forge();
        const result = engagementRecordSchema.safeParse(record);
        expect(result.success).toBe(true);
      });

      it("複数生成した値は全て有効", () => {
        const records = Forger(EngagementRecordMold).forgeMulti(5);
        for (const record of records) {
          const result = engagementRecordSchema.safeParse(record);
          expect(result.success).toBe(true);
        }
      });

      it("dwellTimeが0でも有効", () => {
        const record = Forger(EngagementRecordMold).forge();
        const result = engagementRecordSchema.safeParse({
          ...record,
          dwellTime: 0,
          scrollDepth: 0,
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なEngagementRecordの検証", () => {
      it("identifierが欠けている場合は無効", () => {
        const result = engagementRecordSchema.safeParse({
          dwellTime: 100,
          scrollDepth: 50,
          createdAt: new Date(),
          updatedAt: new Date(),
        });
        expect(result.success).toBe(false);
      });

      it("dwellTimeが範囲外の場合は無効", () => {
        const record = Forger(EngagementRecordMold).forge();
        const result = engagementRecordSchema.safeParse({
          ...record,
          dwellTime: -1,
        });
        expect(result.success).toBe(false);
      });

      it("scrollDepthが範囲外の場合は無効", () => {
        const record = Forger(EngagementRecordMold).forge();
        const result = engagementRecordSchema.safeParse({
          ...record,
          scrollDepth: 101,
        });
        expect(result.success).toBe(false);
      });
    });
  });

  // ---------------------------------------------------------------------------
  // validateEngagementRecord
  // ---------------------------------------------------------------------------
  describe("validateEngagementRecord", () => {
    it("有効なUnvalidatedEngagementRecordでokを返す", () => {
      const identifier = Forger(EngagementRecordIdentifierMold).forge();
      const now = new Date();
      const result = validateEngagementRecord({
        identifier: {
          reference: {
            type: identifier.reference.type,
            content: identifier.reference.content,
          },
          dateKey: identifier.dateKey,
          sessionKey: identifier.sessionKey,
        },
        dwellTime: 120,
        scrollDepth: 75,
        createdAt: now,
        updatedAt: now,
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedEngagementRecordでerrを返す", () => {
      const result = validateEngagementRecord({
        identifier: {
          reference: { type: "invalid", content: "invalid" },
          dateKey: "not-a-date",
          sessionKey: "not-a-uuid",
        },
        dwellTime: -1,
        scrollDepth: 200,
        createdAt: new Date(),
        updatedAt: new Date(),
      });
      expect(result.isErr).toBe(true);
    });
  });

  // ---------------------------------------------------------------------------
  // Criteria
  // ---------------------------------------------------------------------------
  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("全てnullで有効", () => {
        const result = criteriaSchema.safeParse({
          dateRange: null,
          reference: null,
        });
        expect(result.success).toBe(true);
      });

      it("全てundefinedで有効", () => {
        const result = criteriaSchema.safeParse({});
        expect(result.success).toBe(true);
      });

      it("dateRangeのみ指定で有効", () => {
        const now = new Date();
        const past = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
        const result = criteriaSchema.safeParse({
          dateRange: { start: past, end: now },
        });
        expect(result.success).toBe(true);
      });

      it("referenceのみ指定で有効", () => {
        const reference = Forger(SearchReferenceIdentifierMold).forge();
        const result = criteriaSchema.safeParse({
          reference,
        });
        expect(result.success).toBe(true);
      });

      it("全フィールド指定で有効", () => {
        const now = new Date();
        const past = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
        const reference = Forger(SearchReferenceIdentifierMold).forge();
        const result = criteriaSchema.safeParse({
          dateRange: { start: past, end: now },
          reference,
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("dateRangeのendがstartより前の場合は無効", () => {
        const now = new Date();
        const past = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
        const result = criteriaSchema.safeParse({
          dateRange: { start: now, end: past },
        });
        expect(result.success).toBe(false);
      });
    });
  });

  // ---------------------------------------------------------------------------
  // validateCriteria
  // ---------------------------------------------------------------------------
  describe("validateCriteria", () => {
    it("有効なUnvalidatedCriteriaでokを返す", () => {
      const result = validateCriteria({
        dateRange: null,
        reference: null,
      });
      expect(result.isOk).toBe(true);
    });

    it("undefinedフィールドでもokを返す", () => {
      const result = validateCriteria({});
      expect(result.isOk).toBe(true);
    });

    it("dateRangeを指定してもokを返す", () => {
      const now = new Date();
      const past = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
      const result = validateCriteria({
        dateRange: { start: past, end: now },
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なdateRangeでerrを返す", () => {
      const now = new Date();
      const past = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
      const result = validateCriteria({
        dateRange: { start: now, end: past },
      });
      expect(result.isErr).toBe(true);
    });
  });

  // ---------------------------------------------------------------------------
  // formatDwellTime
  // ---------------------------------------------------------------------------
  describe("formatDwellTime", () => {
    it("0秒は'0:00'を返す", () => {
      const dwellTime = dwellTimeSchema.parse(0);
      expect(formatDwellTime(dwellTime)).toBe("0:00");
    });

    it("59秒は'0:59'を返す", () => {
      const dwellTime = dwellTimeSchema.parse(59);
      expect(formatDwellTime(dwellTime)).toBe("0:59");
    });

    it("60秒は'1:00'を返す", () => {
      const dwellTime = dwellTimeSchema.parse(60);
      expect(formatDwellTime(dwellTime)).toBe("1:00");
    });

    it("90秒は'1:30'を返す", () => {
      const dwellTime = dwellTimeSchema.parse(90);
      expect(formatDwellTime(dwellTime)).toBe("1:30");
    });

    it("3661秒は'61:01'を返す", () => {
      const dwellTime = dwellTimeSchema.parse(3661);
      expect(formatDwellTime(dwellTime)).toBe("61:01");
    });

    it("86400秒は'1440:00'を返す", () => {
      const dwellTime = dwellTimeSchema.parse(86_400);
      expect(formatDwellTime(dwellTime)).toBe("1440:00");
    });

    it("秒が1桁の場合は0埋めされる", () => {
      const dwellTime = dwellTimeSchema.parse(65);
      expect(formatDwellTime(dwellTime)).toBe("1:05");
    });
  });
});
