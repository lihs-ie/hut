import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createRecordUniqueVisitorWorkflow,
  createGetUniqueVisitorsWorkflow,
} from "@shared/workflows/analytics/unique-visitor";
import { validateUniqueVisitor } from "@shared/domains/analytics/unique-visitor";
import { validatePeriod } from "@shared/domains/analytics/common";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import {
  duplicationError,
} from "@shared/aspects/error";
import { UniqueVisitorMold } from "../../support/molds/domains/analytics/unique-visitor";
import { Command } from "@shared/workflows/common";

describe("workflows/analytics/unique-visitor", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createRecordUniqueVisitorWorkflow", () => {
    it("バリデーション成功かつ永続化成功でUniqueVisitorRecordedEventを返す", async () => {
      const visitor = Forger(UniqueVisitorMold).forgeWithSeed(1);
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createRecordUniqueVisitorWorkflow(validateUniqueVisitor)(
        persistMock
      )(mockLogger);

      const command: Command<{ sessionKey: string }> = {
        now: visitor.createdAt,
        payload: { sessionKey: visitor.identifier.sessionKey },
      };

      const result = await workflow(command).unwrap();

      expect(result.type).toBe("analytics.uniqueVisitor.recorded");
      expect(result.payload.visitor.identifier.sessionKey).toBe(
        visitor.identifier.sessionKey
      );
      expect(result.occurredAt).toBe(command.now);
      expect(persistMock).toHaveBeenCalled();
    });

    it("バリデーション失敗でValidationErrorを返す", async () => {
      const persistMock = vi.fn();

      const workflow = createRecordUniqueVisitorWorkflow(validateUniqueVisitor)(
        persistMock
      )(mockLogger);

      const command: Command<{ sessionKey: string }> = {
        now: new Date(),
        payload: { sessionKey: "invalid-session-key" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("永続化失敗でDuplicationErrorを返す", async () => {
      const visitor = Forger(UniqueVisitorMold).forgeWithSeed(1);
      const error = duplicationError(
        "UniqueVisitor",
        "Unique visitor already exists"
      );
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createRecordUniqueVisitorWorkflow(validateUniqueVisitor)(
        persistMock
      )(mockLogger);

      const command: Command<{ sessionKey: string }> = {
        now: visitor.createdAt,
        payload: { sessionKey: visitor.identifier.sessionKey },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createGetUniqueVisitorsWorkflow", () => {
    it("有効なperiodでPeriodComparisonを返す", async () => {
      const visitors = Forger(UniqueVisitorMold).forgeMultiWithSeed(5, 1);
      const previousVisitors = Forger(UniqueVisitorMold).forgeMultiWithSeed(
        3,
        10
      );
      const searchMock = vi
        .fn()
        .mockReturnValueOnce(ok(visitors).toAsync())
        .mockReturnValueOnce(ok(previousVisitors).toAsync());

      const workflow = createGetUniqueVisitorsWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(result.current).toBe(5);
      expect(result.previous).toBe(3);
      expect(searchMock).toHaveBeenCalledTimes(2);

      const firstCallArg = searchMock.mock.calls[0][0];
      const secondCallArg = searchMock.mock.calls[1][0];
      expect(firstCallArg).toHaveProperty("dateRange");
      expect(firstCallArg.dateRange).toHaveProperty("start");
      expect(firstCallArg.dateRange).toHaveProperty("end");
      expect(secondCallArg).toHaveProperty("dateRange");
      expect(secondCallArg.dateRange).toHaveProperty("start");
      expect(secondCallArg.dateRange).toHaveProperty("end");
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetUniqueVisitorsWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "invalid" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(searchMock).not.toHaveBeenCalled();
    });
  });
});
