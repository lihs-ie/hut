import { randomUUID } from "crypto";
import { ulid } from "ulid";
import { describe, it, expect, beforeEach, afterAll } from "vitest";
import {
  createAdminFeatureTestContext,
  cleanupAdminFeatureTest,
  testLogger,
} from "../setup";
import {
  getTestFirestoreInstance,
  createFirestoreOperations,
  cleanupTestApp,
} from "../firebase-test-utils";
import type { Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import {
  createRecordPageViewWorkflow,
  createGetTotalPageViewsWorkflow,
  createGetPageViewTrendWorkflow,
  createGetReferrerRankingWorkflow,
  createGetDeviceDistributionWorkflow,
} from "@shared/workflows/analytics/page-view";
import {
  createRecordUniqueVisitorWorkflow,
  createGetUniqueVisitorsWorkflow,
} from "@shared/workflows/analytics/unique-visitor";
import {
  createRecordEngagementWorkflow,
  createGetAverageDwellTimeWorkflow,
  createGetDwellTimeRankingWorkflow,
  createGetScrollDepthDistributionWorkflow,
} from "@shared/workflows/analytics/engagement";
import {
  createRecordSearchWorkflow,
  createGetSearchCountWorkflow,
  createGetSearchKeywordRankingWorkflow,
  createGetSearchCountTrendWorkflow,
  createGetZeroHitKeywordsWorkflow,
} from "@shared/workflows/analytics/search-record";
import {
  createGetContentRankingWorkflow,
  createGetContentTypeComparisonWorkflow,
} from "@shared/workflows/analytics/content";
import { validatePageView } from "@shared/domains/analytics/page-view";
import { validateUniqueVisitor } from "@shared/domains/analytics/unique-visitor";
import { validateEngagementRecord } from "@shared/domains/analytics/engagement";
import { validateSearchRecord } from "@shared/domains/analytics/search-record";
import { validatePeriod } from "@shared/domains/analytics/common";
import { FirebasePageViewRepository } from "@shared/infrastructures/analytics/page-view";
import { FirebaseUniqueVisitorRepository } from "@shared/infrastructures/analytics/unique-visitor";
import { FirebaseEngagementRecordRepository } from "@shared/infrastructures/analytics/engagement";
import { FirebaseSearchRecordRepository } from "@shared/infrastructures/analytics/search-record";
import type { PageViewRepository } from "@shared/domains/analytics/page-view";
import type { UniqueVisitorRepository } from "@shared/domains/analytics/unique-visitor";
import type { EngagementRecordRepository } from "@shared/domains/analytics/engagement";
import type { SearchRecordRepository } from "@shared/domains/analytics/search-record";
import type { ArticleRepository } from "@shared/domains/articles";
import type { MemoRepository } from "@shared/domains/memo";
import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import { FirebaseMemoRepository } from "@shared/infrastructures/memos";

const TEST_APP_NAME = "analytics-feature-test-app";

const NULL_PAGE_VIEW_OPTIONS = { referrer: null, userAgent: null };
const DEFAULT_SEARCH_FILTERS = { tags: null, contentType: null };

describe("Feature: Analytics Actions", () => {
  let firestore: Firestore;
  let operations: FirestoreOperations;
  let pageViewRepository: PageViewRepository;
  let uniqueVisitorRepository: UniqueVisitorRepository;
  let engagementRecordRepository: EngagementRecordRepository;
  let searchRecordRepository: SearchRecordRepository;
  let articleRepository: ArticleRepository;
  let memoRepository: MemoRepository;

  beforeEach(async () => {
    await createAdminFeatureTestContext();
    firestore = getTestFirestoreInstance(TEST_APP_NAME);
    operations = createFirestoreOperations();
    pageViewRepository = FirebasePageViewRepository(firestore, operations);
    uniqueVisitorRepository = FirebaseUniqueVisitorRepository(firestore, operations);
    engagementRecordRepository = FirebaseEngagementRecordRepository(firestore, operations);
    searchRecordRepository = FirebaseSearchRecordRepository(firestore, operations);
    articleRepository = FirebaseArticleRepository(firestore, operations);
    memoRepository = FirebaseMemoRepository(firestore, operations);
  }, 30000);

  afterAll(async () => {
    await cleanupAdminFeatureTest();
    await cleanupTestApp(TEST_APP_NAME);
  }, 30000);

  describe("PageView関連", () => {
    it("PageViewを記録しgetTotalPageViewsでPeriodComparisonを取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      const getTotalWorkflow = createGetTotalPageViewsWorkflow(validatePeriod)(
        pageViewRepository.search
      )(testLogger);

      const result = await getTotalWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(result.current).toBeGreaterThanOrEqual(1);
      expect(typeof result.previous).toBe("number");
    });

    it("PageViewを記録しgetPageViewTrendでTrendPoint[]を取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      const getTrendWorkflow = createGetPageViewTrendWorkflow(validatePeriod)(
        pageViewRepository.search
      )(testLogger);

      const result = await getTrendWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThanOrEqual(1);
      expect(result[0]).toHaveProperty("dateKey");
      expect(result[0]).toHaveProperty("value");
    });

    it("PageViewを記録しgetReferrerRankingでRankedItem[]を取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          referrer: "https://www.google.com/search?q=test",
          userAgent: null,
        },
      }).unwrap();

      const getReferrerWorkflow = createGetReferrerRankingWorkflow(validatePeriod)(
        pageViewRepository.search
      )(testLogger);

      const result = await getReferrerWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThanOrEqual(1);
      expect(result[0]).toHaveProperty("label");
      expect(result[0]).toHaveProperty("value");
    });

    it("PageViewを記録しgetDeviceDistributionでDistribution[]を取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          referrer: null,
          userAgent: "Mozilla/5.0 (iPhone; CPU iPhone OS 15_0 like Mac OS X)",
        },
      }).unwrap();

      const getDeviceWorkflow = createGetDeviceDistributionWorkflow(validatePeriod)(
        pageViewRepository.search
      )(testLogger);

      const result = await getDeviceWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThanOrEqual(1);
      expect(result[0]).toHaveProperty("label");
      expect(result[0]).toHaveProperty("value");
    });
  });

  describe("UniqueVisitor関連", () => {
    it("UniqueVisitorを記録しgetUniqueVisitorsでPeriodComparisonを取得できる", async () => {
      const recordWorkflow = createRecordUniqueVisitorWorkflow(validateUniqueVisitor)(
        uniqueVisitorRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: { sessionKey: randomUUID() },
      }).unwrap();

      const getVisitorsWorkflow = createGetUniqueVisitorsWorkflow(validatePeriod)(
        uniqueVisitorRepository.search
      )(testLogger);

      const result = await getVisitorsWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(result.current).toBeGreaterThanOrEqual(1);
      expect(typeof result.previous).toBe("number");
    });
  });

  describe("Engagement関連", () => {
    it("Engagementを記録しgetAverageDwellTimeでPeriodComparisonを取得できる", async () => {
      const recordWorkflow = createRecordEngagementWorkflow(validateEngagementRecord)(
        engagementRecordRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          dwellTime: 120,
          scrollDepth: 80,
        },
      }).unwrap();

      const getDwellTimeWorkflow = createGetAverageDwellTimeWorkflow(validatePeriod)(
        engagementRecordRepository.search
      )(testLogger);

      const result = await getDwellTimeWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(result.current).toBeGreaterThanOrEqual(1);
      expect(typeof result.previous).toBe("number");
    });

    it("Engagementを記録しgetDwellTimeRankingでRankedItem[]を取得できる", async () => {
      const recordWorkflow = createRecordEngagementWorkflow(validateEngagementRecord)(
        engagementRecordRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          dwellTime: 300,
          scrollDepth: 90,
        },
      }).unwrap();

      const getRankingWorkflow = createGetDwellTimeRankingWorkflow(validatePeriod)(
        engagementRecordRepository.search
      )(articleRepository.search)(memoRepository.search)(testLogger);

      const result = await getRankingWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThanOrEqual(1);
      expect(result[0]).toHaveProperty("label");
      expect(result[0]).toHaveProperty("value");
    });

    it("Engagementを記録しgetScrollDepthDistributionでDistribution[]を取得できる", async () => {
      const recordWorkflow = createRecordEngagementWorkflow(validateEngagementRecord)(
        engagementRecordRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          dwellTime: 60,
          scrollDepth: 50,
        },
      }).unwrap();

      const getDistributionWorkflow = createGetScrollDepthDistributionWorkflow(validatePeriod)(
        engagementRecordRepository.search
      )(testLogger);

      const result = await getDistributionWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(4);
      const labels = result.map((distribution) => distribution.label);
      expect(labels).toContain("0-25%");
      expect(labels).toContain("26-50%");
      expect(labels).toContain("51-75%");
      expect(labels).toContain("76-100%");
    });
  });

  describe("SearchRecord関連", () => {
    it("Searchを記録しgetSearchCountでPeriodComparisonを取得できる", async () => {
      const recordWorkflow = createRecordSearchWorkflow(validateSearchRecord)(
        searchRecordRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: { keyword: "typescript", resultCount: 5, ...DEFAULT_SEARCH_FILTERS },
      }).unwrap();

      const getCountWorkflow = createGetSearchCountWorkflow(validatePeriod)(
        searchRecordRepository.search
      )(testLogger);

      const result = await getCountWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(result.current).toBeGreaterThanOrEqual(1);
      expect(typeof result.previous).toBe("number");
    });

    it("Searchを記録しgetSearchKeywordRankingでRankedItem[]を取得できる", async () => {
      const recordWorkflow = createRecordSearchWorkflow(validateSearchRecord)(
        searchRecordRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: { keyword: "react-hooks", resultCount: 3, ...DEFAULT_SEARCH_FILTERS },
      }).unwrap();

      const getRankingWorkflow = createGetSearchKeywordRankingWorkflow(validatePeriod)(
        searchRecordRepository.search
      )(testLogger);

      const result = await getRankingWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThanOrEqual(1);
      expect(result[0]).toHaveProperty("label");
      expect(result[0]).toHaveProperty("value");
    });

    it("Searchを記録しgetSearchCountTrendでTrendPoint[]を取得できる", async () => {
      const recordWorkflow = createRecordSearchWorkflow(validateSearchRecord)(
        searchRecordRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: { keyword: "nextjs-ssr", resultCount: 7, ...DEFAULT_SEARCH_FILTERS },
      }).unwrap();

      const getTrendWorkflow = createGetSearchCountTrendWorkflow(validatePeriod)(
        searchRecordRepository.search
      )(testLogger);

      const result = await getTrendWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThanOrEqual(1);
      expect(result[0]).toHaveProperty("dateKey");
      expect(result[0]).toHaveProperty("value");
    });

    it("Searchを記録しgetZeroHitKeywordsでRankedItem[]を取得できる", async () => {
      const recordWorkflow = createRecordSearchWorkflow(validateSearchRecord)(
        searchRecordRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: { keyword: "nonexistent-topic", resultCount: 0, ...DEFAULT_SEARCH_FILTERS },
      }).unwrap();

      const getZeroHitWorkflow = createGetZeroHitKeywordsWorkflow(validatePeriod)(
        searchRecordRepository.search
      )(testLogger);

      const result = await getZeroHitWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThanOrEqual(1);
      const labels = result.map((item) => item.label);
      expect(labels).toContain("nonexistent-topic");
    });
  });

  describe("Content関連", () => {
    it("PageViewを記録しgetContentRankingでRankedItem[]を取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      const getContentRankingWorkflow = createGetContentRankingWorkflow(validatePeriod)(
        pageViewRepository.search
      )(articleRepository.search)(memoRepository.search)(testLogger);

      const result = await getContentRankingWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThanOrEqual(1);
      expect(result[0]).toHaveProperty("label");
      expect(result[0]).toHaveProperty("value");
    });

    it("PageViewを記録しgetContentTypeComparisonでDistribution[]を取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      const getTypeComparisonWorkflow = createGetContentTypeComparisonWorkflow(validatePeriod)(
        pageViewRepository.search
      )(testLogger);

      const result = await getTypeComparisonWorkflow({
        now: new Date(),
        payload: { period: "all" },
      }).unwrap();

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThanOrEqual(1);
      expect(result[0]).toHaveProperty("label");
      expect(result[0]).toHaveProperty("value");
    });
  });

  describe("空データ", () => {
    it("空データでgetTotalPageViewsを実行するとcurrent=0,previous=0になる", async () => {
      const getTotalWorkflow = createGetTotalPageViewsWorkflow(validatePeriod)(
        pageViewRepository.search
      )(testLogger);

      const result = await getTotalWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.current).toBe(0);
      expect(result.previous).toBe(0);
    });
  });
});
