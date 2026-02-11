/**
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { randomUUID } from "crypto";
import { ulid } from "ulid";
import {
  createRecordEngagementWorkflow,
  createGetAverageDwellTimeWorkflow,
  createGetDwellTimeRankingWorkflow,
  createGetScrollDepthDistributionWorkflow,
} from "@shared/workflows/analytics/engagement";
import { validateEngagementRecord } from "@shared/domains/analytics/engagement";
import { validatePeriod } from "@shared/domains/analytics/common";
import { FirebaseEngagementRecordRepository } from "@shared/infrastructures/analytics/engagement";
import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import { FirebaseMemoRepository } from "@shared/infrastructures/memos";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../../setup";
import type { EngagementRecordRepository } from "@shared/domains/analytics/engagement";
import type { ArticleRepository } from "@shared/domains/articles";
import type { MemoRepository } from "@shared/domains/memo";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../support/molds/domains/article";
import { MemoMold } from "../../../support/molds/domains/memo";
import { PublishStatus } from "@shared/domains/common";

describe("Feature: Engagement Workflow", () => {
  let context: FeatureTestContext;
  let repository: EngagementRecordRepository;
  let articleRepository: ArticleRepository;
  let memoRepository: MemoRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});
    context = await createFeatureTestContext();
    repository = FirebaseEngagementRecordRepository(
      context.firestore,
      context.operations
    );
    articleRepository = FirebaseArticleRepository(
      context.firestore,
      context.operations
    );
    memoRepository = FirebaseMemoRepository(
      context.firestore,
      context.operations
    );
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("EngagementRecord記録", () => {
    it("EngagementRecordを記録しイベントが返る", async () => {
      const recordWorkflow = createRecordEngagementWorkflow(
        validateEngagementRecord
      )(repository.persist)(testLogger);

      const now = new Date();
      const contentIdentifier = ulid();

      const result = await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: contentIdentifier },
          sessionKey: randomUUID(),
          dwellTime: 120,
          scrollDepth: 80,
        },
      }).unwrap();

      expect(result.type).toBe("analytics.engagement.recorded");
      expect(result.payload.record.dwellTime).toBe(120);
      expect(result.payload.record.scrollDepth).toBe(80);
      expect(result.payload.record.identifier.reference.content).toBe(
        contentIdentifier
      );
      expect(result.occurredAt).toEqual(now);
    });

    it("同一identifierで上書きされる", async () => {
      const recordWorkflow = createRecordEngagementWorkflow(
        validateEngagementRecord
      )(repository.persist)(testLogger);

      const now = new Date();
      const sessionKey = randomUUID();
      const contentIdentifier = ulid();
      const reference = { type: "article", content: contentIdentifier };

      await recordWorkflow({
        now,
        payload: { reference, sessionKey, dwellTime: 60, scrollDepth: 50 },
      }).unwrap();

      const secondResult = await recordWorkflow({
        now,
        payload: { reference, sessionKey, dwellTime: 180, scrollDepth: 90 },
      }).unwrap();

      expect(secondResult.type).toBe("analytics.engagement.recorded");
      expect(secondResult.payload.record.dwellTime).toBe(180);
      expect(secondResult.payload.record.scrollDepth).toBe(90);
    });
  });

  describe("平均滞在時間の取得", () => {
    it("複数記録後にgetAverageDwellTimeで平均が正しく計算される", async () => {
      const recordWorkflow = createRecordEngagementWorkflow(
        validateEngagementRecord
      )(repository.persist)(testLogger);
      const getAverageWorkflow = createGetAverageDwellTimeWorkflow(
        validatePeriod
      )(repository.search)(testLogger);

      const now = new Date();
      const contentIdentifier = ulid();
      const reference = { type: "article", content: contentIdentifier };

      await recordWorkflow({
        now,
        payload: { reference, sessionKey: randomUUID(), dwellTime: 60, scrollDepth: 40 },
      }).unwrap();

      await recordWorkflow({
        now,
        payload: { reference, sessionKey: randomUUID(), dwellTime: 120, scrollDepth: 80 },
      }).unwrap();

      const result = await getAverageWorkflow({
        now,
        payload: { period: "7d" },
      }).unwrap();

      expect(result.current).toBe(90);
    });

    it("データなしでgetAverageDwellTimeを実行するとcurrent=0, previous=0", async () => {
      const getAverageWorkflow = createGetAverageDwellTimeWorkflow(
        validatePeriod
      )(repository.search)(testLogger);

      const result = await getAverageWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.current).toBe(0);
      expect(result.previous).toBe(0);
    });
  });

  describe("滞在時間ランキングの取得", () => {
    it("Articleを登録して記録後にgetDwellTimeRankingでタイトルが解決される", async () => {
      const recordWorkflow = createRecordEngagementWorkflow(
        validateEngagementRecord
      )(repository.persist)(testLogger);

      const articleA = Forger(ArticleMold).forgeWithSeed(100, {
        status: PublishStatus.PUBLISHED,
      });
      const articleB = Forger(ArticleMold).forgeWithSeed(200, {
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(articleA).unwrap();
      await articleRepository.persist(articleB).unwrap();

      const getRankingWorkflow = createGetDwellTimeRankingWorkflow(
        validatePeriod
      )(repository.search)(articleRepository.search)(memoRepository.search)(testLogger);

      const now = new Date();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: articleA.identifier },
          sessionKey: randomUUID(),
          dwellTime: 300,
          scrollDepth: 90,
        },
      }).unwrap();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: articleB.identifier },
          sessionKey: randomUUID(),
          dwellTime: 60,
          scrollDepth: 30,
        },
      }).unwrap();

      const result = await getRankingWorkflow({
        now,
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBeGreaterThanOrEqual(2);
      expect(result[0]?.label).toBe(articleA.title);
      expect(result[0]?.value).toBe(300);
      expect(result[1]?.label).toBe(articleB.title);
      expect(result[1]?.value).toBe(60);
    });

    it("Memoを登録して記録後にgetDwellTimeRankingでMemoタイトルが解決される", async () => {
      const recordWorkflow = createRecordEngagementWorkflow(
        validateEngagementRecord
      )(repository.persist)(testLogger);

      const memo = Forger(MemoMold).forgeWithSeed(300, {
        status: PublishStatus.PUBLISHED,
      });

      await memoRepository.persist(memo).unwrap();

      const getRankingWorkflow = createGetDwellTimeRankingWorkflow(
        validatePeriod
      )(repository.search)(articleRepository.search)(memoRepository.search)(testLogger);

      const now = new Date();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "memo", content: memo.identifier },
          sessionKey: randomUUID(),
          dwellTime: 150,
          scrollDepth: 70,
        },
      }).unwrap();

      const result = await getRankingWorkflow({
        now,
        payload: { period: "7d" },
      }).unwrap();

      const memoEntry = result.find((item) => item.label === memo.title);
      expect(memoEntry).toBeDefined();
      expect(memoEntry!.value).toBe(150);
      expect(memoEntry!.subLabel).toBe("memo");
    });

    it("タイトルが見つからないIDはフォールバックでULIDのまま表示される", async () => {
      const recordWorkflow = createRecordEngagementWorkflow(
        validateEngagementRecord
      )(repository.persist)(testLogger);

      const unknownContentId = ulid();

      const getRankingWorkflow = createGetDwellTimeRankingWorkflow(
        validatePeriod
      )(repository.search)(articleRepository.search)(memoRepository.search)(testLogger);

      const now = new Date();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: unknownContentId },
          sessionKey: randomUUID(),
          dwellTime: 120,
          scrollDepth: 50,
        },
      }).unwrap();

      const result = await getRankingWorkflow({
        now,
        payload: { period: "7d" },
      }).unwrap();

      const entry = result.find((item) => item.label === unknownContentId);
      expect(entry).toBeDefined();
      expect(entry!.value).toBe(120);
    });
  });

  describe("スクロール深度分布の取得", () => {
    it("異なるscrollDepthで記録後にgetScrollDepthDistributionが取得できる", async () => {
      const recordWorkflow = createRecordEngagementWorkflow(
        validateEngagementRecord
      )(repository.persist)(testLogger);
      const getDistributionWorkflow =
        createGetScrollDepthDistributionWorkflow(validatePeriod)(
          repository.search
        )(testLogger);

      const now = new Date();
      const contentIdentifier = ulid();
      const reference = { type: "article", content: contentIdentifier };

      await recordWorkflow({
        now,
        payload: { reference, sessionKey: randomUUID(), dwellTime: 60, scrollDepth: 10 },
      }).unwrap();

      await recordWorkflow({
        now,
        payload: { reference, sessionKey: randomUUID(), dwellTime: 90, scrollDepth: 40 },
      }).unwrap();

      await recordWorkflow({
        now,
        payload: { reference, sessionKey: randomUUID(), dwellTime: 120, scrollDepth: 60 },
      }).unwrap();

      await recordWorkflow({
        now,
        payload: { reference, sessionKey: randomUUID(), dwellTime: 150, scrollDepth: 85 },
      }).unwrap();

      const result = await getDistributionWorkflow({
        now,
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBe(4);
      expect(result[0]?.label).toBe("0-25%");
      expect(result[0]?.value).toBe(1);
      expect(result[1]?.label).toBe("26-50%");
      expect(result[1]?.value).toBe(1);
      expect(result[2]?.label).toBe("51-75%");
      expect(result[2]?.value).toBe(1);
      expect(result[3]?.label).toBe("76-100%");
      expect(result[3]?.value).toBe(1);
    });
  });

  describe("バリデーションエラー", () => {
    it("無効なperiodでValidationError", async () => {
      const getAverageWorkflow = createGetAverageDwellTimeWorkflow(
        validatePeriod
      )(repository.search)(testLogger);

      const result = await getAverageWorkflow({
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
