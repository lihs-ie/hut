/**
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { randomUUID } from "crypto";
import {
  createRecordUniqueVisitorWorkflow,
  createGetUniqueVisitorsWorkflow,
} from "@shared/workflows/analytics/unique-visitor";
import { validateUniqueVisitor } from "@shared/domains/analytics/unique-visitor";
import { validatePeriod } from "@shared/domains/analytics/common";
import { FirebaseUniqueVisitorRepository } from "@shared/infrastructures/analytics/unique-visitor";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../../setup";
import type { UniqueVisitorRepository } from "@shared/domains/analytics/unique-visitor";

describe("Feature: UniqueVisitor Workflow", () => {
  let context: FeatureTestContext;
  let repository: UniqueVisitorRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});
    context = await createFeatureTestContext();
    repository = FirebaseUniqueVisitorRepository(
      context.firestore,
      context.operations
    );
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("UniqueVisitorの記録", () => {
    it("UniqueVisitorを記録しイベントが返る", async () => {
      const recordWorkflow = createRecordUniqueVisitorWorkflow(
        validateUniqueVisitor
      )(repository.persist)(testLogger);

      const now = new Date();
      const sessionKey = randomUUID();

      const result = await recordWorkflow({
        now,
        payload: { sessionKey },
      }).unwrap();

      expect(result.type).toBe("analytics.uniqueVisitor.recorded");
      expect(result.payload.visitor.identifier.sessionKey).toBe(sessionKey);
      expect(result.occurredAt).toBeDefined();
    });

    it("同一sessionKey+dateKeyの重複記録でDuplicationError", async () => {
      const recordWorkflow = createRecordUniqueVisitorWorkflow(
        validateUniqueVisitor
      )(repository.persist)(testLogger);

      const now = new Date();
      const sessionKey = randomUUID();
      const payload = { sessionKey };

      await recordWorkflow({ now, payload }).unwrap();

      const duplicateResult = await recordWorkflow({ now, payload }).match({
        ok: () => ({ duplicated: false }),
        err: () => ({ duplicated: true }),
      });

      expect(duplicateResult.duplicated).toBe(true);
    });
  });

  describe("UniqueVisitorの取得", () => {
    it("記録後にgetUniqueVisitorsで現在期間のカウントが取得できる", async () => {
      const recordWorkflow = createRecordUniqueVisitorWorkflow(
        validateUniqueVisitor
      )(repository.persist)(testLogger);
      const getWorkflow = createGetUniqueVisitorsWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      const now = new Date();

      await recordWorkflow({
        now,
        payload: { sessionKey: randomUUID() },
      }).unwrap();

      const comparison = await getWorkflow({
        now,
        payload: { period: "7d" },
      }).unwrap();

      expect(comparison.current).toBeGreaterThanOrEqual(1);
      expect(typeof comparison.previous).toBe("number");
    });

    it("データなしでgetUniqueVisitorsを実行するとcurrent=0, previous=0", async () => {
      const getWorkflow = createGetUniqueVisitorsWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      const comparison = await getWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(comparison.current).toBe(0);
      expect(comparison.previous).toBe(0);
    });
  });

  describe("バリデーションエラー", () => {
    it("無効なperiodでValidationError", async () => {
      const getWorkflow = createGetUniqueVisitorsWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      const result = await getWorkflow({
        now: new Date(),
        payload: { period: "invalid" },
      }).match({
        ok: () => ({ valid: true }),
        err: () => ({ valid: false }),
      });

      expect(result.valid).toBe(false);
    });
  });
});
