/**
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { randomUUID } from "crypto";
import { ulid } from "ulid";
import {
  createGetContentRankingWorkflow,
  createGetTagPageViewsWorkflow,
  createGetContentTypeComparisonWorkflow,
} from "@shared/workflows/analytics/content";
import { createRecordPageViewWorkflow } from "@shared/workflows/analytics/page-view";
import { validatePageView } from "@shared/domains/analytics/page-view";
import { validatePeriod } from "@shared/domains/analytics/common";
import { FirebasePageViewRepository } from "@shared/infrastructures/analytics/page-view";
import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../../setup";
import type { PageViewRepository } from "@shared/domains/analytics/page-view";
import type { ArticleRepository } from "@shared/domains/articles";
import type { MemoRepository } from "@shared/domains/memo";
import { FirebaseMemoRepository } from "@shared/infrastructures/memos";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../support/molds/domains/article";
import { MemoMold } from "../../../support/molds/domains/memo";
import { TagIdentifierMold } from "../../../support/molds/domains/attributes/tag";
import { PublishStatus } from "@shared/domains/common";

const NULL_PAGE_VIEW_OPTIONS = { referrer: null, userAgent: null };

describe("Feature: Content Workflow", () => {
  let context: FeatureTestContext;
  let pageViewRepository: PageViewRepository;
  let articleRepository: ArticleRepository;
  let memoRepository: MemoRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});
    context = await createFeatureTestContext();
    pageViewRepository = FirebasePageViewRepository(
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

  describe("ContentRankingの取得", () => {
    it("Articleを登録してPageView記録後にgetContentRankingでタイトルが解決される", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);

      const articleA = Forger(ArticleMold).forgeWithSeed(100, {
        status: PublishStatus.PUBLISHED,
      });
      const articleB = Forger(ArticleMold).forgeWithSeed(200, {
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(articleA).unwrap();
      await articleRepository.persist(articleB).unwrap();

      const getContentRankingWorkflow =
        createGetContentRankingWorkflow(validatePeriod)(
          pageViewRepository.search
        )(articleRepository.search)(memoRepository.search)(testLogger);

      const now = new Date();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: articleA.identifier },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: articleA.identifier },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: articleB.identifier },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      const result = await getContentRankingWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBeGreaterThanOrEqual(2);
      const articleAEntry = result.find(
        (item) => item.label === articleA.title
      );
      const articleBEntry = result.find(
        (item) => item.label === articleB.title
      );
      expect(articleAEntry).toBeDefined();
      expect(articleBEntry).toBeDefined();
      expect(articleAEntry!.value).toBe(2);
      expect(articleBEntry!.value).toBe(1);
      expect(articleAEntry!.subLabel).toBe("article");
      expect(result[0]?.label).toBe(articleA.title);
    });

    it("Memoを登録してPageView記録後にgetContentRankingでMemoタイトルが解決される", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);

      const memo = Forger(MemoMold).forgeWithSeed(300, {
        status: PublishStatus.PUBLISHED,
      });

      await memoRepository.persist(memo).unwrap();

      const getContentRankingWorkflow =
        createGetContentRankingWorkflow(validatePeriod)(
          pageViewRepository.search
        )(articleRepository.search)(memoRepository.search)(testLogger);

      const now = new Date();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "memo", content: memo.identifier },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      const result = await getContentRankingWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      const memoEntry = result.find((item) => item.label === memo.title);
      expect(memoEntry).toBeDefined();
      expect(memoEntry!.value).toBe(1);
      expect(memoEntry!.subLabel).toBe("memo");
    });

    it("タイトルが見つからないIDはフォールバックでULIDのまま表示される", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);

      const unknownContentId = ulid();

      const getContentRankingWorkflow =
        createGetContentRankingWorkflow(validatePeriod)(
          pageViewRepository.search
        )(articleRepository.search)(memoRepository.search)(testLogger);

      const now = new Date();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: unknownContentId },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      const result = await getContentRankingWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      const entry = result.find((item) => item.label === unknownContentId);
      expect(entry).toBeDefined();
      expect(entry!.value).toBe(1);
    });
  });

  describe("TagPageViewsの取得", () => {
    it("ArticleにtagとPageView記録後にgetTagPageViewsで取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);
      const getTagPageViewsWorkflow = createGetTagPageViewsWorkflow(
        validatePeriod
      )(pageViewRepository.search)(articleRepository.search)(testLogger);

      const tag1 = Forger(TagIdentifierMold).forgeWithSeed(1);
      const tag2 = Forger(TagIdentifierMold).forgeWithSeed(2);
      const article = Forger(ArticleMold).forgeWithSeed(1, {
        tags: [tag1, tag2],
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(article).unwrap();

      const now = new Date();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: article.identifier },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: article.identifier },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      const result = await getTagPageViewsWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBeGreaterThanOrEqual(2);
      const tag1Entry = result.find((item) => item.label === tag1);
      const tag2Entry = result.find((item) => item.label === tag2);
      expect(tag1Entry).toBeDefined();
      expect(tag2Entry).toBeDefined();
      expect(tag1Entry!.value).toBe(2);
      expect(tag2Entry!.value).toBe(2);
    });
  });

  describe("ContentTypeComparisonの取得", () => {
    it("article/memoのPageView記録後にgetContentTypeComparisonで分布が取得できる", async () => {
      const recordWorkflow = createRecordPageViewWorkflow(validatePageView)(
        pageViewRepository.persist
      )(testLogger);
      const getContentTypeComparisonWorkflow =
        createGetContentTypeComparisonWorkflow(validatePeriod)(
          pageViewRepository.search
        )(testLogger);

      const now = new Date();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "article", content: ulid() },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      await recordWorkflow({
        now,
        payload: {
          reference: { type: "memo", content: ulid() },
          sessionKey: randomUUID(),
          ...NULL_PAGE_VIEW_OPTIONS,
        },
      }).unwrap();

      const result = await getContentTypeComparisonWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result.length).toBe(2);
      const articleEntry = result.find((item) => item.label === "article");
      const memoEntry = result.find((item) => item.label === "memo");
      expect(articleEntry).toBeDefined();
      expect(memoEntry).toBeDefined();
      expect(articleEntry!.value).toBe(2);
      expect(memoEntry!.value).toBe(1);
      expect(result[0]?.label).toBe("article");
    });
  });

  describe("データなしのケース", () => {
    it("データなしでgetContentRankingを実行すると空配列", async () => {
      const getContentRankingWorkflow =
        createGetContentRankingWorkflow(validatePeriod)(
          pageViewRepository.search
        )(articleRepository.search)(memoRepository.search)(testLogger);

      const result = await getContentRankingWorkflow({
        now: new Date(),
        payload: { period: "7d" },
      }).unwrap();

      expect(result).toEqual([]);
    });
  });

  describe("バリデーションエラー", () => {
    it("無効なperiodでValidationError", async () => {
      const getContentRankingWorkflow =
        createGetContentRankingWorkflow(validatePeriod)(
          pageViewRepository.search
        )(articleRepository.search)(memoRepository.search)(testLogger);

      const result = await getContentRankingWorkflow({
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
