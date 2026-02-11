import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import {
  unexpectedError,
  duplicationError,
  type ValidationError,
} from "@shared/aspects/error";
import {
  validatePeriod,
  dateKeySchema,
} from "@shared/domains/analytics/common";
import { PageViewMold } from "../../support/molds/domains/analytics/page-view";
import { Command } from "@shared/workflows/common";
import {
  createRecordPageViewWorkflow,
  createGetTotalPageViewsWorkflow,
  createGetPageViewTrendWorkflow,
  createGetReferrerRankingWorkflow,
  createGetDeviceDistributionWorkflow,
} from "@shared/workflows/analytics/page-view";

describe("workflows/analytics/page-view", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createRecordPageViewWorkflow", () => {
    it("有効なPageViewデータでPV記録を作成できる", async () => {
      const pageView = Forger(PageViewMold).forgeWithSeed(1);
      const validateMock = vi.fn().mockReturnValue(ok(pageView));
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createRecordPageViewWorkflow(validateMock)(persistMock)(
        mockLogger
      );

      const command: Command<{
        reference: { type: string; content: string };
        sessionKey: string;
        referrer: string | null;
        userAgent: string | null;
      }> = {
        now: new Date(),
        payload: {
          reference: {
            type: pageView.identifier.reference.type,
            content: pageView.identifier.reference.content,
          },
          sessionKey: pageView.identifier.sessionKey,
          referrer: pageView.referrer.raw,
          userAgent: "Mozilla/5.0",
        },
      };

      const result = await workflow(command).unwrap();

      expect(result.type).toBe("analytics.pageView.recorded");
      expect(result.payload.pageView).toEqual(pageView);
      expect(result.occurredAt).toBeDefined();
      expect(persistMock).toHaveBeenCalledWith(pageView);
    });

    it("バリデーションエラー時にValidationErrorを返す", async () => {
      const validationErrors: ValidationError[] = [
        {
          _tag: Symbol.for("ValidationError"),
          field: "identifier",
          description: "Invalid identifier",
        },
      ];
      const validateMock = vi.fn().mockReturnValue(err(validationErrors));
      const persistMock = vi.fn();

      const workflow = createRecordPageViewWorkflow(validateMock)(persistMock)(
        mockLogger
      );

      const command: Command<{
        reference: { type: string; content: string };
        sessionKey: string;
        referrer: string | null;
        userAgent: string | null;
      }> = {
        now: new Date(),
        payload: {
          reference: { type: "article", content: "test" },
          sessionKey: "invalid",
          referrer: null,
          userAgent: null,
        },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("永続化エラー時にエラーを返す", async () => {
      const pageView = Forger(PageViewMold).forgeWithSeed(2);
      const validateMock = vi.fn().mockReturnValue(ok(pageView));
      const error = duplicationError("PageView", "Duplicate page view");
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createRecordPageViewWorkflow(validateMock)(persistMock)(
        mockLogger
      );

      const command: Command<{
        reference: { type: string; content: string };
        sessionKey: string;
        referrer: string | null;
        userAgent: string | null;
      }> = {
        now: new Date(),
        payload: {
          reference: {
            type: pageView.identifier.reference.type,
            content: pageView.identifier.reference.content,
          },
          sessionKey: pageView.identifier.sessionKey,
          referrer: pageView.referrer.raw,
          userAgent: null,
        },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createGetTotalPageViewsWorkflow", () => {
    it("有効なperiodで合計PV数のPeriodComparisonを返す", async () => {
      const currentPageViews = Forger(PageViewMold).forgeMultiWithSeed(5, 1);
      const previousPageViews = Forger(PageViewMold).forgeMultiWithSeed(3, 10);

      const searchMock = vi
        .fn()
        .mockReturnValueOnce(ok(currentPageViews).toAsync())
        .mockReturnValueOnce(ok(previousPageViews).toAsync());

      const workflow = createGetTotalPageViewsWorkflow(validatePeriod)(
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
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetTotalPageViewsWorkflow(validatePeriod)(
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

    it("検索エラー時にUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Search failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createGetTotalPageViewsWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "30d" },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createGetPageViewTrendWorkflow", () => {
    it("有効なperiodでTrendPointの配列を返す", async () => {
      const dateKey1 = dateKeySchema.parse("2024-01-15");
      const dateKey2 = dateKeySchema.parse("2024-01-16");

      const pageView1 = Forger(PageViewMold).forgeWithSeed(1);
      const pageView2 = Forger(PageViewMold).forgeWithSeed(2);
      const pageView3 = Forger(PageViewMold).forgeWithSeed(3);

      const pageViews = [
        { ...pageView1, identifier: { ...pageView1.identifier, dateKey: dateKey1 } },
        { ...pageView2, identifier: { ...pageView2.identifier, dateKey: dateKey1 } },
        { ...pageView3, identifier: { ...pageView3.identifier, dateKey: dateKey2 } },
      ];

      const searchMock = vi.fn().mockReturnValue(ok(pageViews).toAsync());

      const workflow = createGetPageViewTrendWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(result.length).toBe(2);
      const sorted = [...result].sort((first, second) =>
        first.dateKey.localeCompare(second.dateKey)
      );
      expect(sorted[0].dateKey).toBe("2024-01-15");
      expect(sorted[0].value).toBe(2);
      expect(sorted[1].dateKey).toBe("2024-01-16");
      expect(sorted[1].value).toBe(1);
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetPageViewTrendWorkflow(validatePeriod)(
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

  describe("createGetReferrerRankingWorkflow", () => {
    it("有効なperiodでRankedItemの配列を返す", async () => {
      const pageView1 = Forger(PageViewMold).forgeWithSeed(1);
      const pageView2 = Forger(PageViewMold).forgeWithSeed(2);
      const pageView3 = Forger(PageViewMold).forgeWithSeed(4);

      const searchMock = vi
        .fn()
        .mockReturnValue(ok([pageView1, pageView2, pageView3]).toAsync());

      const workflow = createGetReferrerRankingWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThan(0);
      for (let index = 0; index < result.length - 1; index++) {
        expect(result[index].value).toBeGreaterThanOrEqual(
          result[index + 1].value
        );
      }
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetReferrerRankingWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "bad" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(searchMock).not.toHaveBeenCalled();
    });
  });

  describe("createGetDeviceDistributionWorkflow", () => {
    it("有効なperiodでDistributionの配列を返す", async () => {
      const pageView1 = Forger(PageViewMold).forgeWithSeed(1);
      const pageView2 = Forger(PageViewMold).forgeWithSeed(2);
      const pageView3 = Forger(PageViewMold).forgeWithSeed(3);

      const searchMock = vi
        .fn()
        .mockReturnValue(ok([pageView1, pageView2, pageView3]).toAsync());

      const workflow = createGetDeviceDistributionWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThan(0);
      for (let index = 0; index < result.length - 1; index++) {
        expect(result[index].value).toBeGreaterThanOrEqual(
          result[index + 1].value
        );
      }
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetDeviceDistributionWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "xyz" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(searchMock).not.toHaveBeenCalled();
    });

    it("検索エラー時にUnexpectedErrorを返す", async () => {
      const error = unexpectedError("DB connection failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createGetDeviceDistributionWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "90d" },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });
});
