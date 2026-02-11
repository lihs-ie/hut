import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import { unexpectedError } from "@shared/aspects/error";
import {
  SearchRecordMold,
  SearchKeywordMold,
} from "../../support/molds/domains/analytics/search-record";
import { Command } from "@shared/workflows/common";
import {
  validatePeriod,
  dateKeySchema,
} from "@shared/domains/analytics/common";
import {
  createRecordSearchWorkflow,
  createGetSearchCountWorkflow,
  createGetSearchKeywordRankingWorkflow,
  createGetSearchCountTrendWorkflow,
  createGetZeroHitKeywordsWorkflow,
} from "@shared/workflows/analytics/search-record";

describe("workflows/analytics/search-record", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createRecordSearchWorkflow", () => {
    it("有効なSearchRecordデータで検索ログを記録できる", async () => {
      const searchRecord = Forger(SearchRecordMold).forgeWithSeed(1);
      const validateMock = vi.fn().mockReturnValue(ok(searchRecord));
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createRecordSearchWorkflow(validateMock)(persistMock)(
        mockLogger,
      );

      const command: Command<{
        keyword: string;
        resultCount: number;
        tags: string[] | null;
        contentType: string | null;
      }> = {
        now: new Date(),
        payload: {
          keyword: searchRecord.keyword,
          resultCount: searchRecord.resultCount,
          tags: searchRecord.tags,
          contentType: searchRecord.contentType,
        },
      };

      const result = await workflow(command).unwrap();

      expect(result.type).toBe("analytics.search.recorded");
      expect(result.payload.record).toEqual(searchRecord);
      expect(result.occurredAt).toBeDefined();
      expect(persistMock).toHaveBeenCalledWith(searchRecord);
    });

    it("バリデーションエラー時にValidationErrorを返す", async () => {
      const validationErrors = [
        {
          _tag: Symbol.for("ValidationError"),
          field: "keyword",
          description: "Invalid keyword",
        },
      ];
      const validateMock = vi.fn().mockReturnValue(err(validationErrors));
      const persistMock = vi.fn();

      const workflow = createRecordSearchWorkflow(validateMock)(persistMock)(
        mockLogger,
      );

      const command: Command<{
        keyword: string;
        resultCount: number;
        tags: string[] | null;
        contentType: string | null;
      }> = {
        now: new Date(),
        payload: {
          keyword: "",
          resultCount: 0,
          tags: null,
          contentType: null,
        },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true,
      );
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("永続化エラー時にUnexpectedErrorを返す", async () => {
      const searchRecord = Forger(SearchRecordMold).forgeWithSeed(2);
      const validateMock = vi.fn().mockReturnValue(ok(searchRecord));
      const error = unexpectedError("Persist failed");
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createRecordSearchWorkflow(validateMock)(persistMock)(
        mockLogger,
      );

      const command: Command<{
        keyword: string;
        resultCount: number;
        tags: string[] | null;
        contentType: string | null;
      }> = {
        now: new Date(),
        payload: {
          keyword: searchRecord.keyword,
          resultCount: searchRecord.resultCount,
          tags: searchRecord.tags,
          contentType: searchRecord.contentType,
        },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createGetSearchCountWorkflow", () => {
    it("有効なperiodで検索回数のPeriodComparisonを返す", async () => {
      const currentRecords = Forger(SearchRecordMold).forgeMultiWithSeed(5, 1);
      const previousRecords = Forger(SearchRecordMold).forgeMultiWithSeed(
        3,
        10,
      );

      const searchMock = vi
        .fn()
        .mockReturnValueOnce(ok(currentRecords).toAsync())
        .mockReturnValueOnce(ok(previousRecords).toAsync());

      const workflow = createGetSearchCountWorkflow(validatePeriod)(searchMock)(
        mockLogger,
      );

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

      const workflow = createGetSearchCountWorkflow(validatePeriod)(searchMock)(
        mockLogger,
      );

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "invalid" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true,
      );
      expect(searchMock).not.toHaveBeenCalled();
    });

    it("検索エラー時にUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Search failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createGetSearchCountWorkflow(validatePeriod)(searchMock)(
        mockLogger,
      );

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "30d" },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createGetSearchKeywordRankingWorkflow", () => {
    it("有効なperiodで複数キーワードをvalue降順のRankedItem配列で返す", async () => {
      const keyword1 = Forger(SearchKeywordMold).forgeWithSeed(1);
      const keyword2 = Forger(SearchKeywordMold).forgeWithSeed(2);

      const record1 = Forger(SearchRecordMold).forgeWithSeed(1, {
        keyword: keyword1,
      });
      const record2 = Forger(SearchRecordMold).forgeWithSeed(2, {
        keyword: keyword1,
      });
      const record3 = Forger(SearchRecordMold).forgeWithSeed(3, {
        keyword: keyword1,
      });
      const record4 = Forger(SearchRecordMold).forgeWithSeed(4, {
        keyword: keyword2,
      });

      const searchMock = vi
        .fn()
        .mockReturnValue(ok([record1, record2, record3, record4]).toAsync());

      const workflow = createGetSearchKeywordRankingWorkflow(validatePeriod)(
        searchMock,
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(2);
      expect(result[0].value).toBe(3);
      expect(result[0].label).toBe(keyword1);
      expect(result[1].value).toBe(1);
      expect(result[1].label).toBe(keyword2);
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetSearchKeywordRankingWorkflow(validatePeriod)(
        searchMock,
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "bad" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true,
      );
      expect(searchMock).not.toHaveBeenCalled();
    });
  });

  describe("createGetSearchCountTrendWorkflow", () => {
    it("有効なperiodで日付別集計のTrendPoint配列を返す", async () => {
      const dateKey1 = dateKeySchema.parse("2024-01-15");
      const dateKey2 = dateKeySchema.parse("2024-01-16");

      const record1 = Forger(SearchRecordMold).forgeWithSeed(1, {
        dateKey: dateKey1,
      });
      const record2 = Forger(SearchRecordMold).forgeWithSeed(2, {
        dateKey: dateKey1,
      });
      const record3 = Forger(SearchRecordMold).forgeWithSeed(3, {
        dateKey: dateKey2,
      });

      const searchMock = vi
        .fn()
        .mockReturnValue(ok([record1, record2, record3]).toAsync());

      const workflow = createGetSearchCountTrendWorkflow(validatePeriod)(
        searchMock,
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(result.length).toBe(2);
      const sorted = [...result].sort((first, second) =>
        first.dateKey.localeCompare(second.dateKey),
      );
      expect(sorted[0].dateKey).toBe("2024-01-15");
      expect(sorted[0].value).toBe(2);
      expect(sorted[1].dateKey).toBe("2024-01-16");
      expect(sorted[1].value).toBe(1);
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetSearchCountTrendWorkflow(validatePeriod)(
        searchMock,
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "invalid" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true,
      );
      expect(searchMock).not.toHaveBeenCalled();
    });
  });

  describe("createGetZeroHitKeywordsWorkflow", () => {
    it("有効なperiodでhasResults: falseの検索をRankedItem配列で返す", async () => {
      const keyword1 = Forger(SearchKeywordMold).forgeWithSeed(10);
      const keyword2 = Forger(SearchKeywordMold).forgeWithSeed(20);

      const record1 = Forger(SearchRecordMold).forgeWithSeed(10, {
        keyword: keyword1,
        resultCount: 0,
      });
      const record2 = Forger(SearchRecordMold).forgeWithSeed(11, {
        keyword: keyword1,
        resultCount: 0,
      });
      const record3 = Forger(SearchRecordMold).forgeWithSeed(12, {
        keyword: keyword2,
        resultCount: 0,
      });

      const searchMock = vi
        .fn()
        .mockReturnValue(ok([record1, record2, record3]).toAsync());

      const workflow = createGetZeroHitKeywordsWorkflow(validatePeriod)(
        searchMock,
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(2);
      expect(result[0].value).toBe(2);
      expect(result[0].label).toBe(keyword1);
      expect(result[1].value).toBe(1);
      expect(result[1].label).toBe(keyword2);

      const searchCall = searchMock.mock.calls[0][0];
      expect(searchCall.hasResults).toBe(false);
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetZeroHitKeywordsWorkflow(validatePeriod)(
        searchMock,
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "xyz" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true,
      );
      expect(searchMock).not.toHaveBeenCalled();
    });

    it("検索エラー時にUnexpectedErrorを返す", async () => {
      const error = unexpectedError("DB connection failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createGetZeroHitKeywordsWorkflow(validatePeriod)(
        searchMock,
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
