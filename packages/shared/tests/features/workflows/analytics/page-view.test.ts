/**
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { randomUUID } from "crypto";
import { ulid } from "ulid";
import {
  createRecordPageViewWorkflow,
  createGetTotalPageViewsWorkflow,
  createGetPageViewTrendWorkflow,
  createGetReferrerRankingWorkflow,
  createGetDeviceDistributionWorkflow,
} from "@shared/workflows/analytics/page-view";
import { validatePageView } from "@shared/domains/analytics/page-view";
import { validatePeriod, toJstDateKey } from "@shared/domains/analytics/common";
import { FirebasePageViewRepository } from "@shared/infrastructures/analytics/page-view";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../../setup";
import type { PageViewRepository } from "@shared/domains/analytics/page-view";

describe("Feature: PageView Workflow", () => {
  let context: FeatureTestContext;
  let repository: PageViewRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});
    context = await createFeatureTestContext();
    repository = FirebasePageViewRepository(
      context.firestore,
      context.operations
    );
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("PageView記録ワークフロー", () => {
    it("PageViewを記録しイベントが返る", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        repository.persist
      )(testLogger);

      const now = new Date();
      const result = await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          referrer: "https://google.com",
          userAgent:
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
        },
      }).unwrap();

      expect(result.type).toBe("analytics.pageView.recorded");
      expect(result.identifier).toBeDefined();
      expect(result.occurredAt).toEqual(now);
      expect(result.payload.pageView).toBeDefined();
      expect(result.payload.pageView.referrer.raw).toBe(
        "https://google.com"
      );
    });

    it("同一session+date+referenceの重複記録でerrを返す", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        repository.persist
      )(testLogger);

      const now = new Date();
      const reference = { type: "article", content: ulid() };
      const sessionKey = randomUUID();
      const payload = { reference, sessionKey, referrer: null, userAgent: null };

      await recordWorkflow({ now, payload }).unwrap();

      const duplicateResult = await recordWorkflow({ now, payload }).match({
        ok: () => ({ duplicated: false }),
        err: () => ({ duplicated: true }),
      });

      expect(duplicateResult.duplicated).toBe(true);
    });

    it("userAgentからdeviceTypeが正しく判定される (mobile)", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        repository.persist
      )(testLogger);

      const result = await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          referrer: null,
          userAgent:
            "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X) AppleWebKit/605.1.15",
        },
      }).unwrap();

      expect(result.payload.pageView.deviceType).toBe("mobile");
    });
  });

  describe("PageView取得ワークフロー", () => {
    it("記録後にgetTotalPageViewsで現在期間のカウントが取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        repository.persist
      )(testLogger);
      const getTotalWorkflow = createGetTotalPageViewsWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          referrer: null,
          userAgent: null,
        },
      }).unwrap();

      const result = await getTotalWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.current).toBeGreaterThanOrEqual(1);
      expect(typeof result.previous).toBe("number");
    });

    it("記録後にgetPageViewTrendで日別カウントが取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        repository.persist
      )(testLogger);
      const getTrendWorkflow = createGetPageViewTrendWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      const now = new Date();
      const expectedDateKey = toJstDateKey(now);

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          referrer: null,
          userAgent: null,
        },
      }).unwrap();

      const result = await getTrendWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBeGreaterThanOrEqual(1);
      const todayPoint = result.find(
        (point) => point.dateKey === expectedDateKey
      );
      expect(todayPoint).toBeDefined();
      expect(todayPoint?.value).toBeGreaterThanOrEqual(1);
    });

    it("異なるreferrerで記録後にgetReferrerRankingで降順ランキングが取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        repository.persist
      )(testLogger);
      const getRankingWorkflow =
        createGetReferrerRankingWorkflow(validatePeriod)(repository.search)(
          testLogger
        );

      const contentId = ulid();

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: contentId },
          sessionKey: randomUUID(),
          referrer: "https://google.com/search?q=test",
          userAgent: null,
        },
      }).unwrap();

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: contentId },
          sessionKey: randomUUID(),
          referrer: "https://google.com/search?q=another",
          userAgent: null,
        },
      }).unwrap();

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: contentId },
          sessionKey: randomUUID(),
          referrer: "https://twitter.com/status/123",
          userAgent: null,
        },
      }).unwrap();

      const result = await getRankingWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBeGreaterThanOrEqual(2);
      const googleEntry = result.find(
        (item) => item.label === "google.com"
      );
      const twitterEntry = result.find(
        (item) => item.label === "twitter.com"
      );
      expect(googleEntry).toBeDefined();
      expect(twitterEntry).toBeDefined();
      expect(googleEntry!.value).toBeGreaterThan(twitterEntry!.value);
    });

    it("異なるdeviceTypeで記録後にgetDeviceDistributionで分布が取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        repository.persist
      )(testLogger);
      const getDistributionWorkflow =
        createGetDeviceDistributionWorkflow(validatePeriod)(repository.search)(
          testLogger
        );

      const contentId = ulid();

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: contentId },
          sessionKey: randomUUID(),
          referrer: null,
          userAgent:
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
        },
      }).unwrap();

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: contentId },
          sessionKey: randomUUID(),
          referrer: null,
          userAgent:
            "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X) AppleWebKit/605.1.15",
        },
      }).unwrap();

      await recordWorkflow({
        now: new Date(),
        payload: {
          reference: { type: "article", content: contentId },
          sessionKey: randomUUID(),
          referrer: null,
          userAgent:
            "Mozilla/5.0 (iPad; CPU OS 16_0 like Mac OS X) AppleWebKit/605.1.15",
        },
      }).unwrap();

      const result = await getDistributionWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBe(3);
      const desktopEntry = result.find((item) => item.label === "desktop");
      const mobileEntry = result.find((item) => item.label === "mobile");
      const tabletEntry = result.find((item) => item.label === "tablet");
      expect(desktopEntry).toBeDefined();
      expect(mobileEntry).toBeDefined();
      expect(tabletEntry).toBeDefined();
      expect(desktopEntry!.value).toBe(1);
      expect(mobileEntry!.value).toBe(1);
      expect(tabletEntry!.value).toBe(1);
    });
  });

  describe("データなしのケース", () => {
    it("データなしでgetTotalPageViewsを実行するとcurrent=0, previous=0を返す", async () => {
      const getTotalWorkflow = createGetTotalPageViewsWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      const result = await getTotalWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.current).toBe(0);
      expect(result.previous).toBe(0);
    });
  });

  describe("バリデーションエラー", () => {
    it("無効なperiodでValidationErrorを返す", async () => {
      const getTotalWorkflow = createGetTotalPageViewsWorkflow(validatePeriod)(
        repository.search
      )(testLogger);

      const result = await getTotalWorkflow({
        now: new Date(),
        payload: { period: "invalid-period" },
      }).match({
        ok: () => ({ valid: true }),
        err: () => ({ valid: false }),
      });

      expect(result.valid).toBe(false);
    });
  });
});
