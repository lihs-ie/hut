import { describe, it, expect } from "vitest";
import {
  uniqueVisitorIdentifierSchema,
  UniqueVisitorIdentifier,
  equalUniqueVisitorIdentifier,
  uniqueVisitorSchema,
  UniqueVisitor,
  criteriaSchema,
  Criteria,
  validateUniqueVisitor,
  validateUniqueVisitorIdentifier,
  validateCriteria,
  UnvalidatedUniqueVisitor,
} from "@shared/domains/analytics/unique-visitor";
// ---------------------------------------------------------------------------
// Helper: create valid raw data
// ---------------------------------------------------------------------------

const validDateKey = "2025-01-15";
const validSessionKey = () => crypto.randomUUID();

const createValidIdentifierInput = () => ({
  dateKey: validDateKey,
  sessionKey: validSessionKey(),
});

const createValidUniqueVisitorInput = (): UnvalidatedUniqueVisitor => ({
  identifier: createValidIdentifierInput(),
  createdAt: new Date(),
});

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe("domains/analytics/unique-visitor", () => {
  // -----------------------------------------------------------------------
  // UniqueVisitorIdentifier schema
  // -----------------------------------------------------------------------
  describe("uniqueVisitorIdentifierSchema", () => {
    describe("有効なUniqueVisitorIdentifierの検証", () => {
      it("有効なdateKeyとsessionKeyで検証を通過する", () => {
        const input = createValidIdentifierInput();
        const result = uniqueVisitorIdentifierSchema.safeParse(input);
        expect(result.success).toBe(true);
      });

      it("複数の異なるsessionKeyで検証を通過する", () => {
        for (let index = 0; index < 5; index++) {
          const input = createValidIdentifierInput();
          const result = uniqueVisitorIdentifierSchema.safeParse(input);
          expect(result.success).toBe(true);
        }
      });
    });

    describe("無効なUniqueVisitorIdentifierの検証", () => {
      it("dateKeyが不正な形式の場合は無効", () => {
        const result = uniqueVisitorIdentifierSchema.safeParse({
          dateKey: "2025/01/15",
          sessionKey: validSessionKey(),
        });
        expect(result.success).toBe(false);
      });

      it("dateKeyが空文字の場合は無効", () => {
        const result = uniqueVisitorIdentifierSchema.safeParse({
          dateKey: "",
          sessionKey: validSessionKey(),
        });
        expect(result.success).toBe(false);
      });

      it("sessionKeyが不正なUUIDの場合は無効", () => {
        const result = uniqueVisitorIdentifierSchema.safeParse({
          dateKey: validDateKey,
          sessionKey: "invalid-uuid",
        });
        expect(result.success).toBe(false);
      });

      it("sessionKeyが空文字の場合は無効", () => {
        const result = uniqueVisitorIdentifierSchema.safeParse({
          dateKey: validDateKey,
          sessionKey: "",
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = uniqueVisitorIdentifierSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = uniqueVisitorIdentifierSchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });

      it("dateKeyが欠けている場合は無効", () => {
        const result = uniqueVisitorIdentifierSchema.safeParse({
          sessionKey: validSessionKey(),
        });
        expect(result.success).toBe(false);
      });

      it("sessionKeyが欠けている場合は無効", () => {
        const result = uniqueVisitorIdentifierSchema.safeParse({
          dateKey: validDateKey,
        });
        expect(result.success).toBe(false);
      });
    });
  });

  // -----------------------------------------------------------------------
  // equalUniqueVisitorIdentifier
  // -----------------------------------------------------------------------
  describe("equalUniqueVisitorIdentifier", () => {
    it("同一のdateKeyとsessionKeyであればtrueを返す", () => {
      const session = validSessionKey();
      const left = uniqueVisitorIdentifierSchema.parse({
        dateKey: validDateKey,
        sessionKey: session,
      });
      const right = uniqueVisitorIdentifierSchema.parse({
        dateKey: validDateKey,
        sessionKey: session,
      });
      expect(equalUniqueVisitorIdentifier(left, right)).toBe(true);
    });

    it("dateKeyが異なればfalseを返す", () => {
      const session = validSessionKey();
      const left = uniqueVisitorIdentifierSchema.parse({
        dateKey: "2025-01-15",
        sessionKey: session,
      });
      const right = uniqueVisitorIdentifierSchema.parse({
        dateKey: "2025-01-16",
        sessionKey: session,
      });
      expect(equalUniqueVisitorIdentifier(left, right)).toBe(false);
    });

    it("sessionKeyが異なればfalseを返す", () => {
      const left = uniqueVisitorIdentifierSchema.parse({
        dateKey: validDateKey,
        sessionKey: validSessionKey(),
      });
      const right = uniqueVisitorIdentifierSchema.parse({
        dateKey: validDateKey,
        sessionKey: validSessionKey(),
      });
      expect(equalUniqueVisitorIdentifier(left, right)).toBe(false);
    });
  });

  // -----------------------------------------------------------------------
  // UniqueVisitor schema
  // -----------------------------------------------------------------------
  describe("uniqueVisitorSchema", () => {
    describe("有効なUniqueVisitorの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const input = {
          identifier: createValidIdentifierInput(),
          createdAt: new Date(),
        };
        const result = uniqueVisitorSchema.safeParse(input);
        expect(result.success).toBe(true);
      });
    });

    describe("無効なUniqueVisitorの検証", () => {
      it("identifierが無効な場合は無効", () => {
        const result = uniqueVisitorSchema.safeParse({
          identifier: { dateKey: "invalid", sessionKey: "invalid" },
          createdAt: new Date(),
        });
        expect(result.success).toBe(false);
      });

      it("createdAtがDateでない場合は無効", () => {
        const result = uniqueVisitorSchema.safeParse({
          identifier: createValidIdentifierInput(),
          createdAt: "not-a-date",
        });
        expect(result.success).toBe(false);
      });

      it("createdAtが欠けている場合は無効", () => {
        const result = uniqueVisitorSchema.safeParse({
          identifier: createValidIdentifierInput(),
        });
        expect(result.success).toBe(false);
      });

      it("nullは無効", () => {
        const result = uniqueVisitorSchema.safeParse(null);
        expect(result.success).toBe(false);
      });

      it("undefinedは無効", () => {
        const result = uniqueVisitorSchema.safeParse(undefined);
        expect(result.success).toBe(false);
      });
    });
  });

  // -----------------------------------------------------------------------
  // Criteria schema
  // -----------------------------------------------------------------------
  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("dateRangeがnullで有効", () => {
        const result = criteriaSchema.safeParse({ dateRange: null });
        expect(result.success).toBe(true);
      });

      it("dateRangeがundefinedで有効", () => {
        const result = criteriaSchema.safeParse({});
        expect(result.success).toBe(true);
      });

      it("dateRangeが有効な日付範囲で有効", () => {
        const result = criteriaSchema.safeParse({
          dateRange: {
            start: new Date("2025-01-01"),
            end: new Date("2025-01-31"),
          },
        });
        expect(result.success).toBe(true);
      });

      it("dateRangeのstartとendが同じ日付で有効", () => {
        const sameDate = new Date("2025-01-15");
        const result = criteriaSchema.safeParse({
          dateRange: {
            start: sameDate,
            end: sameDate,
          },
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("dateRangeのendがstartより前の場合は無効", () => {
        const result = criteriaSchema.safeParse({
          dateRange: {
            start: new Date("2025-01-31"),
            end: new Date("2025-01-01"),
          },
        });
        expect(result.success).toBe(false);
      });

      it("dateRangeのstartがDateでない場合は無効", () => {
        const result = criteriaSchema.safeParse({
          dateRange: {
            start: "not-a-date",
            end: new Date("2025-01-31"),
          },
        });
        expect(result.success).toBe(false);
      });
    });
  });

  // -----------------------------------------------------------------------
  // validateUniqueVisitorIdentifier
  // -----------------------------------------------------------------------
  describe("validateUniqueVisitorIdentifier", () => {
    it("有効な入力でokを返す", () => {
      const result = validateUniqueVisitorIdentifier(
        createValidIdentifierInput(),
      );
      expect(result.isOk).toBe(true);
    });

    it("無効な入力でerrを返す", () => {
      const result = validateUniqueVisitorIdentifier({
        dateKey: "invalid",
        sessionKey: "invalid",
      });
      expect(result.isErr).toBe(true);
    });
  });

  // -----------------------------------------------------------------------
  // validateUniqueVisitor
  // -----------------------------------------------------------------------
  describe("validateUniqueVisitor", () => {
    it("有効なUnvalidatedUniqueVisitorでokを返す", () => {
      const result = validateUniqueVisitor(createValidUniqueVisitorInput());
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedUniqueVisitorでerrを返す", () => {
      const result = validateUniqueVisitor({
        identifier: { dateKey: "invalid", sessionKey: "invalid" },
        createdAt: new Date(),
      });
      expect(result.isErr).toBe(true);
    });

    it("createdAtが欠けている場合にerrを返す", () => {
      const input = {
        identifier: createValidIdentifierInput(),
      } as UnvalidatedUniqueVisitor;
      const result = validateUniqueVisitor(input);
      expect(result.isErr).toBe(true);
    });
  });

  // -----------------------------------------------------------------------
  // validateCriteria
  // -----------------------------------------------------------------------
  describe("validateCriteria", () => {
    it("有効なCriteriaでokを返す", () => {
      const result = validateCriteria({
        dateRange: {
          start: new Date("2025-01-01"),
          end: new Date("2025-01-31"),
        },
      });
      expect(result.isOk).toBe(true);
    });

    it("dateRangeがnullでokを返す", () => {
      const result = validateCriteria({ dateRange: null });
      expect(result.isOk).toBe(true);
    });

    it("空オブジェクトでokを返す", () => {
      const result = validateCriteria({});
      expect(result.isOk).toBe(true);
    });

    it("無効なdateRangeでerrを返す", () => {
      const result = validateCriteria({
        dateRange: {
          start: new Date("2025-01-31"),
          end: new Date("2025-01-01"),
        },
      });
      expect(result.isErr).toBe(true);
    });
  });

  // -----------------------------------------------------------------------
  // 型エクスポートの確認 (コンパイル時チェック)
  // -----------------------------------------------------------------------
  describe("型エクスポートの確認", () => {
    it("UniqueVisitorIdentifier型が利用可能", () => {
      const identifier: UniqueVisitorIdentifier =
        uniqueVisitorIdentifierSchema.parse(createValidIdentifierInput());
      expect(identifier.dateKey).toBe(validDateKey);
    });

    it("UniqueVisitor型が利用可能", () => {
      const visitor: UniqueVisitor = uniqueVisitorSchema.parse({
        identifier: createValidIdentifierInput(),
        createdAt: new Date(),
      });
      expect(visitor.createdAt).toBeInstanceOf(Date);
    });

    it("Criteria型が利用可能", () => {
      const criteria: Criteria = criteriaSchema.parse({
        dateRange: null,
      });
      expect(criteria.dateRange).toBeNull();
    });
  });
});
