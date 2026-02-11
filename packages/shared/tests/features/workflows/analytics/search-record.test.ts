/**
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import {
  createRecordSearchWorkflow,
  createGetSearchCountWorkflow,
  createGetSearchKeywordRankingWorkflow,
  createGetSearchCountTrendWorkflow,
  createGetZeroHitKeywordsWorkflow,
} from "@shared/workflows/analytics/search-record";
import { validateSearchRecord } from "@shared/domains/analytics/search-record";
import { validatePeriod, toJstDateKey } from "@shared/domains/analytics/common";
import { FirebaseSearchRecordRepository } from "@shared/infrastructures/analytics/search-record";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../../setup";
import type { SearchRecordRepository } from "@shared/domains/analytics/search-record";

const DEFAULT_SEARCH_FILTERS = { tags: null, contentType: null };

describe("Feature: SearchRecord Workflow", () => {
  let context: FeatureTestContext;
  let repository: SearchRecordRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});
    context = await createFeatureTestContext();
    repository = FirebaseSearchRecordRepository(
      context.firestore,
      context.operations
    );
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("SearchRecord記録", () => {
    it("SearchRecordを記録しイベントが返る", async () => {
      const recordWorkflow = createRecordSearchWorkflow(validateSearchRecord)(
        repository.persist
      )(testLogger);

      const now = new Date();

      const result = await recordWorkflow({
        now,
        payload: { keyword: "React", resultCount: 5, ...DEFAULT_SEARCH_FILTERS },
      }).unwrap();

      expect(result.type).toBe("analytics.search.recorded");
      expect(result.payload.record.keyword).toBe("React");
      expect(result.payload.record.resultCount).toBe(5);
      expect(result.payload.record.dateKey).toBe(toJstDateKey(now));
      expect(result.occurredAt).toEqual(now);
    });
  });

  describe("検索件数の取得", () => {
    it("複数記録後にgetSearchCountで件数が取得できる", async () => {
      const recordWorkflow = createRecordSearchWorkflow(validateSearchRecord)(
        repository.persist
      )(testLogger);
      const getCountWorkflow = createGetSearchCountWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      const now = new Date();

      await Promise.all([
        recordWorkflow({
          now,
          payload: { keyword: "React", resultCount: 10, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
        recordWorkflow({
          now,
          payload: { keyword: "Next.js", resultCount: 3, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
        recordWorkflow({
          now,
          payload: { keyword: "TypeScript", resultCount: 7, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
      ]);

      const result = await getCountWorkflow({
        now,
        payload: { period: "7d" },
      }).unwrap();

      expect(result.current).toBe(3);
    });

    it("データなしでgetSearchCountを実行するとcurrent=0, previous=0", async () => {
      const getCountWorkflow = createGetSearchCountWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      const result = await getCountWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.current).toBe(0);
      expect(result.previous).toBe(0);
    });
  });

  describe("キーワードランキングの取得", () => {
    it("同一キーワードで複数回記録後にgetSearchKeywordRankingで降順ランキング", async () => {
      const recordWorkflow = createRecordSearchWorkflow(validateSearchRecord)(
        repository.persist
      )(testLogger);
      const getRankingWorkflow = createGetSearchKeywordRankingWorkflow(
        validatePeriod
      )(repository.search)(testLogger);

      const now = new Date();

      await Promise.all([
        recordWorkflow({
          now,
          payload: { keyword: "React", resultCount: 5, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
        recordWorkflow({
          now,
          payload: { keyword: "React", resultCount: 3, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
        recordWorkflow({
          now,
          payload: { keyword: "React", resultCount: 8, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
        recordWorkflow({
          now,
          payload: { keyword: "Next.js", resultCount: 2, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
      ]);

      const result = await getRankingWorkflow({
        now,
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBeGreaterThanOrEqual(2);
      expect(result[0]?.label).toBe("React");
      expect(result[0]?.value).toBe(3);
      expect(result[1]?.label).toBe("Next.js");
      expect(result[1]?.value).toBe(1);
    });
  });

  describe("検索件数トレンドの取得", () => {
    it("記録後にgetSearchCountTrendで日別集計が取得できる", async () => {
      const recordWorkflow = createRecordSearchWorkflow(validateSearchRecord)(
        repository.persist
      )(testLogger);
      const getTrendWorkflow = createGetSearchCountTrendWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      const now = new Date();

      await Promise.all([
        recordWorkflow({
          now,
          payload: { keyword: "React", resultCount: 5, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
        recordWorkflow({
          now,
          payload: { keyword: "Next.js", resultCount: 3, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
      ]);

      const result = await getTrendWorkflow({
        now,
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBeGreaterThanOrEqual(1);
      const todayKey = toJstDateKey(now);
      const todayPoint = result.find((point) => point.dateKey === todayKey);
      expect(todayPoint).toBeDefined();
      expect(todayPoint?.value).toBe(2);
    });
  });

  describe("ゼロヒットキーワードの取得", () => {
    it("resultCount=0で記録後にgetZeroHitKeywordsで取得できる", async () => {
      const recordWorkflow = createRecordSearchWorkflow(validateSearchRecord)(
        repository.persist
      )(testLogger);
      const getZeroHitWorkflow = createGetZeroHitKeywordsWorkflow(
        validatePeriod
      )(repository.search)(testLogger);

      const now = new Date();

      await Promise.all([
        recordWorkflow({
          now,
          payload: { keyword: "存在しないキーワード", resultCount: 0, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
        recordWorkflow({
          now,
          payload: { keyword: "見つからない検索語", resultCount: 0, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
        recordWorkflow({
          now,
          payload: { keyword: "React", resultCount: 10, ...DEFAULT_SEARCH_FILTERS },
        }).unwrap(),
      ]);

      const result = await getZeroHitWorkflow({
        now,
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBe(2);
      const labels = result.map((item) => item.label);
      expect(labels).toContain("存在しないキーワード");
      expect(labels).toContain("見つからない検索語");
      expect(labels).not.toContain("React");
    });
  });

  describe("バリデーションエラー", () => {
    it("無効なperiodでValidationError", async () => {
      const getCountWorkflow = createGetSearchCountWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      const result = await getCountWorkflow({
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
