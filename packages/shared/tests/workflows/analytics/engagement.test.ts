import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import { unexpectedError, ValidationError } from "@shared/aspects/error";
import {
  EngagementRecordMold,
} from "../../support/molds/domains/analytics/engagement";
import { Command } from "@shared/workflows/common";
import {
  createRecordEngagementWorkflow,
  createGetAverageDwellTimeWorkflow,
  createGetDwellTimeRankingWorkflow,
  createGetScrollDepthDistributionWorkflow,
} from "@shared/workflows/analytics/engagement";
import { validatePeriod } from "@shared/domains/analytics/common";
import {
  EngagementRecord,
  engagementRecordIdentifierSchema,
  dwellTimeSchema,
  scrollDepthSchema,
  engagementRecordSchema,
} from "@shared/domains/analytics/engagement";
import {
  searchReferenceIdentifierSchema,
} from "@shared/domains/search-token/reference";
import {
  dateKeySchema,
  sessionKeySchema,
} from "@shared/domains/analytics/common";
import { articleIdentifierSchema } from "@shared/domains/articles";
import { ulid } from "ulid";

const createEngagementRecordWithOverrides = (overrides: {
  content?: string;
  type?: string;
  dwellTime?: number;
  scrollDepth?: number;
}): EngagementRecord => {
  const now = new Date();
  const contentValue =
    overrides.content ?? articleIdentifierSchema.parse(ulid());
  const typeValue = overrides.type ?? "article";
  const reference = searchReferenceIdentifierSchema.parse({
    type: typeValue,
    content: contentValue,
  });
  const dateKey = dateKeySchema.parse("2024-01-15");
  const sessionKey = sessionKeySchema.parse(crypto.randomUUID());
  const identifier = engagementRecordIdentifierSchema.parse({
    reference,
    dateKey,
    sessionKey,
  });

  return engagementRecordSchema.parse({
    identifier,
    dwellTime: dwellTimeSchema.parse(overrides.dwellTime ?? 120),
    scrollDepth: scrollDepthSchema.parse(overrides.scrollDepth ?? 50),
    createdAt: now,
    updatedAt: now,
  });
};

describe("workflows/analytics/engagement", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createRecordEngagementWorkflow", () => {
    it("有効なEngagementRecordデータでエンゲージメント記録を作成できる", async () => {
      const record = Forger(EngagementRecordMold).forgeWithSeed(1);
      const validateMock = vi.fn().mockReturnValue(ok(record));
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createRecordEngagementWorkflow(validateMock)(persistMock)(
        mockLogger
      );

      const command: Command<{
        reference: { type: string; content: string };
        sessionKey: string;
        dwellTime: number;
        scrollDepth: number;
      }> = {
        now: new Date(),
        payload: {
          reference: {
            type: record.identifier.reference.type,
            content: record.identifier.reference.content,
          },
          sessionKey: record.identifier.sessionKey,
          dwellTime: record.dwellTime,
          scrollDepth: record.scrollDepth,
        },
      };

      const result = await workflow(command).unwrap();

      expect(result.type).toBe("analytics.engagement.recorded");
      expect(result.payload.record).toEqual(record);
      expect(result.occurredAt).toBeDefined();
      expect(persistMock).toHaveBeenCalledWith(record);
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

      const workflow = createRecordEngagementWorkflow(validateMock)(persistMock)(
        mockLogger
      );

      const command: Command<{
        reference: { type: string; content: string };
        sessionKey: string;
        dwellTime: number;
        scrollDepth: number;
      }> = {
        now: new Date(),
        payload: {
          reference: { type: "article", content: "test" },
          sessionKey: "invalid",
          dwellTime: 100,
          scrollDepth: 50,
        },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("永続化エラー時にUnexpectedErrorを返す", async () => {
      const record = Forger(EngagementRecordMold).forgeWithSeed(2);
      const validateMock = vi.fn().mockReturnValue(ok(record));
      const error = unexpectedError("Persist failed");
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createRecordEngagementWorkflow(validateMock)(persistMock)(
        mockLogger
      );

      const command: Command<{
        reference: { type: string; content: string };
        sessionKey: string;
        dwellTime: number;
        scrollDepth: number;
      }> = {
        now: new Date(),
        payload: {
          reference: {
            type: record.identifier.reference.type,
            content: record.identifier.reference.content,
          },
          sessionKey: record.identifier.sessionKey,
          dwellTime: record.dwellTime,
          scrollDepth: record.scrollDepth,
        },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createGetAverageDwellTimeWorkflow", () => {
    it("有効なperiodで平均滞在時間のPeriodComparisonを返す", async () => {
      const currentRecords = [
        createEngagementRecordWithOverrides({ dwellTime: 100 }),
        createEngagementRecordWithOverrides({ dwellTime: 200 }),
        createEngagementRecordWithOverrides({ dwellTime: 300 }),
      ];
      const previousRecords = [
        createEngagementRecordWithOverrides({ dwellTime: 50 }),
        createEngagementRecordWithOverrides({ dwellTime: 150 }),
      ];

      const searchMock = vi
        .fn()
        .mockReturnValueOnce(ok(currentRecords).toAsync())
        .mockReturnValueOnce(ok(previousRecords).toAsync());

      const workflow = createGetAverageDwellTimeWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(result.current).toBe(200);
      expect(result.previous).toBe(100);
      expect(searchMock).toHaveBeenCalledTimes(2);
    });

    it("空データ時はcurrent: 0, previous: 0を返す", async () => {
      const searchMock = vi
        .fn()
        .mockReturnValueOnce(ok([]).toAsync())
        .mockReturnValueOnce(ok([]).toAsync());

      const workflow = createGetAverageDwellTimeWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "30d" },
      };

      const result = await workflow(command).unwrap();

      expect(result.current).toBe(0);
      expect(result.previous).toBe(0);
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetAverageDwellTimeWorkflow(validatePeriod)(
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

      const workflow = createGetAverageDwellTimeWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createGetDwellTimeRankingWorkflow", () => {
    it("有効なperiodでコンテンツ別平均滞在時間ランキングを返す", async () => {
      const contentA = articleIdentifierSchema.parse(ulid());
      const contentB = articleIdentifierSchema.parse(ulid());

      const records = [
        createEngagementRecordWithOverrides({
          content: contentA,
          type: "article",
          dwellTime: 300,
        }),
        createEngagementRecordWithOverrides({
          content: contentA,
          type: "article",
          dwellTime: 100,
        }),
        createEngagementRecordWithOverrides({
          content: contentB,
          type: "article",
          dwellTime: 500,
        }),
      ];

      const searchMock = vi
        .fn()
        .mockReturnValue(ok(records).toAsync());

      const searchArticlesMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const searchMemosMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const workflow = createGetDwellTimeRankingWorkflow(validatePeriod)(
        searchMock
      )(searchArticlesMock)(searchMemosMock)(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(2);
      expect(result[0].value).toBe(500);
      expect(result[0].label).toBe(contentB);
      expect(result[0].subLabel).toBe("article");
      expect(result[1].value).toBe(200);
      expect(result[1].label).toBe(contentA);
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const searchArticlesMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const searchMemosMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const workflow = createGetDwellTimeRankingWorkflow(validatePeriod)(
        searchMock
      )(searchArticlesMock)(searchMemosMock)(mockLogger);

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

    it("検索エラー時にUnexpectedErrorを返す", async () => {
      const error = unexpectedError("DB connection failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const searchArticlesMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const searchMemosMock = vi.fn().mockReturnValue(ok([]).toAsync());
      const workflow = createGetDwellTimeRankingWorkflow(validatePeriod)(
        searchMock
      )(searchArticlesMock)(searchMemosMock)(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createGetScrollDepthDistributionWorkflow", () => {
    it("有効なperiodでスクロール深度の分布を返す", async () => {
      const records = [
        createEngagementRecordWithOverrides({ scrollDepth: 10 }),
        createEngagementRecordWithOverrides({ scrollDepth: 25 }),
        createEngagementRecordWithOverrides({ scrollDepth: 40 }),
        createEngagementRecordWithOverrides({ scrollDepth: 60 }),
        createEngagementRecordWithOverrides({ scrollDepth: 80 }),
        createEngagementRecordWithOverrides({ scrollDepth: 95 }),
      ];

      const searchMock = vi
        .fn()
        .mockReturnValue(ok(records).toAsync());

      const workflow = createGetScrollDepthDistributionWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "7d" },
      };

      const result = await workflow(command).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(4);

      const bucket025 = result.find(
        (distribution) => distribution.label === "0-25%"
      );
      const bucket2650 = result.find(
        (distribution) => distribution.label === "26-50%"
      );
      const bucket5175 = result.find(
        (distribution) => distribution.label === "51-75%"
      );
      const bucket76100 = result.find(
        (distribution) => distribution.label === "76-100%"
      );

      expect(bucket025?.value).toBe(2);
      expect(bucket2650?.value).toBe(1);
      expect(bucket5175?.value).toBe(1);
      expect(bucket76100?.value).toBe(2);
    });

    it("空データ時は全バケットvalue: 0を返す", async () => {
      const searchMock = vi
        .fn()
        .mockReturnValue(ok([]).toAsync());

      const workflow = createGetScrollDepthDistributionWorkflow(validatePeriod)(
        searchMock
      )(mockLogger);

      const command: Command<{ period: string }> = {
        now: new Date(),
        payload: { period: "30d" },
      };

      const result = await workflow(command).unwrap();

      expect(result.length).toBe(4);
      for (const distribution of result) {
        expect(distribution.value).toBe(0);
      }
    });

    it("無効なperiodでValidationErrorを返す", async () => {
      const searchMock = vi.fn();

      const workflow = createGetScrollDepthDistributionWorkflow(validatePeriod)(
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
      const error = unexpectedError("Search failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createGetScrollDepthDistributionWorkflow(validatePeriod)(
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
