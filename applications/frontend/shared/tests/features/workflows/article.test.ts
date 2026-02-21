/**
 * Article Workflow Feature Test
 *
 * Firebase Emulatorを使用してArticleワークフローの統合テストを行います。
 */

import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createArticleFindWorkflow,
  createArticleFindBySlugWorkflow,
  createArticleSearchWorkflow,
  createArticleCreateWorkflow,
  createArticleTerminateWorkflow,
} from "@shared/workflows/article";
import {
  validateArticle,
  validateArticleIdentifier,
  validateCriteria,
} from "@shared/domains/articles";
import { validateSlug } from "@shared/domains/common/slug";
import { PublishStatus } from "@shared/domains/common";
import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../setup";
import {
  ArticleMold,
  ArticleIdentifierMold,
  SlugMold,
} from "../../support/molds/domains/article";
import { TagIdentifierMold } from "../../support/molds/domains/attributes/tag";
import type { Article, ArticleRepository } from "@shared/domains/articles";

function toArticlePayload(article: Article) {
  return {
    identifier: article.identifier,
    title: article.title,
    content: article.content,
    excerpt: article.excerpt,
    slug: article.slug,
    status: article.status,
    tags: article.tags,
    images: article.images,
    timeline: article.timeline,
  };
}

describe("Feature: Article Workflow (実DB接続)", () => {
  let context: FeatureTestContext;
  let repository: ArticleRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    context = await createFeatureTestContext();
    repository = FirebaseArticleRepository(context.firestore, context.operations);
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("記事作成から検索までの一連の流れ", () => {
    it("新規記事を作成し、その記事を検索して取得できる", async () => {
      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        repository.persist
      )(testLogger);
      const searchWorkflow = createArticleSearchWorkflow(validateCriteria)(
        repository.search
      )(testLogger);

      const article = Forger(ArticleMold).forgeWithSeed(1, {
        status: PublishStatus.PUBLISHED,
      });

      const createResult = await createWorkflow({
        now: new Date(),
        payload: toArticlePayload(article),
      }).unwrap();

      expect(createResult.payload.snapshot.identifier).toBe(article.identifier);
      expect(createResult.occurredAt).toBeDefined();

      const searchResult = await searchWorkflow({
        now: new Date(),
        payload: { status: PublishStatus.PUBLISHED },
      }).unwrap();

      expect(searchResult.length).toBeGreaterThanOrEqual(1);
      expect(
        searchResult.some((item) => item.identifier === article.identifier)
      ).toBe(true);
    });

    it("記事を作成後、identifierで取得できる", async () => {
      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        repository.persist
      )(testLogger);
      const findWorkflow = createArticleFindWorkflow(validateArticleIdentifier)(
        repository.find
      )(testLogger);

      const article = Forger(ArticleMold).forgeWithSeed(2);

      await createWorkflow({
        now: new Date(),
        payload: toArticlePayload(article),
      }).unwrap();

      const result = await findWorkflow({
        now: new Date(),
        payload: { identifier: article.identifier },
      }).unwrap();

      expect(result.identifier).toBe(article.identifier);
      expect(result.title).toBe(article.title);
      expect(result.content).toBe(article.content);
    });
  });

  describe("記事検索ワークフロー", () => {
    it("identifierで記事を取得できる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(10);

      await repository.persist(article).unwrap();

      const findWorkflow = createArticleFindWorkflow(validateArticleIdentifier)(
        repository.find
      )(testLogger);

      const result = await findWorkflow({
        now: new Date(),
        payload: { identifier: article.identifier },
      }).unwrap();

      expect(result.identifier).toBe(article.identifier);
      expect(result.title).toBe(article.title);
      expect(result.content).toBe(article.content);
    });

    it("slugで記事を取得できる", async () => {
      const slug = Forger(SlugMold).forgeWithSeed(20, {
        value: "feature-test-slug",
      });
      const article = Forger(ArticleMold).forgeWithSeed(21, { slug });

      await repository.persist(article).unwrap();

      const findBySlugWorkflow = createArticleFindBySlugWorkflow(validateSlug)(
        testLogger
      )(repository.findBySlug);

      const result = await findBySlugWorkflow({
        now: new Date(),
        payload: { slug },
      }).unwrap();

      expect(result.identifier).toBe(article.identifier);
      expect(result.slug).toBe(slug);
    });

    it("タグで記事を検索できる", async () => {
      const targetTag = Forger(TagIdentifierMold).forgeWithSeed(30);
      const anotherTag = Forger(TagIdentifierMold).forgeWithSeed(31);
      const differentTag = Forger(TagIdentifierMold).forgeWithSeed(32);

      const article1 = Forger(ArticleMold).forgeWithSeed(30, {
        tags: [targetTag, anotherTag],
        status: PublishStatus.PUBLISHED,
      });
      const article2 = Forger(ArticleMold).forgeWithSeed(31, {
        tags: [differentTag],
        status: PublishStatus.PUBLISHED,
      });

      await repository.persist(article1).unwrap();
      await repository.persist(article2).unwrap();

      const searchWorkflow = createArticleSearchWorkflow(validateCriteria)(
        repository.search
      )(testLogger);

      const result = await searchWorkflow({
        now: new Date(),
        payload: { tags: [targetTag] },
      }).unwrap();

      expect(result.length).toBe(1);
      expect(result[0]?.identifier).toBe(article1.identifier);
    });

    it("ステータスで記事を検索できる", async () => {
      const publishedArticle = Forger(ArticleMold).forgeWithSeed(40, {
        status: PublishStatus.PUBLISHED,
      });
      const draftArticle = Forger(ArticleMold).forgeWithSeed(41, {
        status: PublishStatus.DRAFT,
      });

      await repository.persist(publishedArticle).unwrap();
      await repository.persist(draftArticle).unwrap();

      const searchWorkflow = createArticleSearchWorkflow(validateCriteria)(
        repository.search
      )(testLogger);

      const result = await searchWorkflow({
        now: new Date(),
        payload: { status: PublishStatus.PUBLISHED },
      }).unwrap();

      expect(result.length).toBe(1);
      expect(result[0]?.identifier).toBe(publishedArticle.identifier);
    });
  });

  describe("記事削除ワークフロー", () => {
    it("記事を削除でき、その後検索で見つからない", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(50);

      await repository.persist(article).unwrap();

      const terminateWorkflow = createArticleTerminateWorkflow(
        validateArticleIdentifier
      )(repository.terminate)(testLogger);
      const findWorkflow = createArticleFindWorkflow(validateArticleIdentifier)(
        repository.find
      )(testLogger);

      const terminateResult = await terminateWorkflow({
        now: new Date(),
        payload: { identifier: article.identifier },
      }).unwrap();

      expect(terminateResult.payload.article).toBe(article.identifier);
      expect(terminateResult.occurredAt).toBeDefined();

      const findResult = await findWorkflow({
        now: new Date(),
        payload: { identifier: article.identifier },
      }).match({
        ok: () => "found",
        err: () => "not-found",
      });

      expect(findResult).toBe("not-found");
    });
  });

  describe("並列処理の最適化", () => {
    it("複数記事の作成が並列で実行できる", async () => {
      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        repository.persist
      )(testLogger);

      const articles = Forger(ArticleMold).forgeMultiWithSeed(3, 60);

      const results = await Promise.all(
        articles.map((article) =>
          createWorkflow({
            now: new Date(),
            payload: toArticlePayload(article),
          }).unwrap()
        )
      );

      expect(results.length).toBe(3);
      results.forEach((result, index) => {
        expect(result.payload.snapshot.identifier).toBe(
          articles[index]?.identifier
        );
      });

      // 永続化されたことを確認
      for (const article of articles) {
        const found = await repository.find(article.identifier).unwrap();
        expect(found.identifier).toBe(article.identifier);
      }
    });
  });

  describe("エラーハンドリング", () => {
    it("存在しない記事を検索するとAggregateNotFoundErrorが返る", async () => {
      const findWorkflow = createArticleFindWorkflow(validateArticleIdentifier)(
        repository.find
      )(testLogger);

      const nonExistentIdentifier =
        Forger(ArticleIdentifierMold).forgeWithSeed(100);

      const result = await findWorkflow({
        now: new Date(),
        payload: { identifier: nonExistentIdentifier },
      }).match({
        ok: () => ({ found: true }),
        err: (error) => ({ found: false, error }),
      });

      expect(result.found).toBe(false);
    });

    it("無効なidentifierでValidationErrorが返る", async () => {
      const findWorkflow = createArticleFindWorkflow(validateArticleIdentifier)(
        repository.find
      )(testLogger);

      const result = await findWorkflow({
        now: new Date(),
        payload: { identifier: "invalid-identifier" },
      }).match({
        ok: () => ({ valid: true }),
        err: () => ({ valid: false }),
      });

      expect(result.valid).toBe(false);
    });
  });

  describe("データ永続化の検証", () => {
    it("作成した記事のデータが正確に永続化される", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(110, {
        status: PublishStatus.PUBLISHED,
      });

      await repository.persist(article).unwrap();

      const found = await repository.find(article.identifier).unwrap();

      expect(found.identifier).toBe(article.identifier);
      expect(found.title).toBe(article.title);
      expect(found.content).toBe(article.content);
      expect(found.excerpt).toBe(article.excerpt);
      expect(found.slug).toBe(article.slug);
      expect(found.status).toBe(article.status);
      expect(found.tags).toEqual(article.tags);
      expect(found.timeline.createdAt.getTime()).toBe(
        article.timeline.createdAt.getTime()
      );
      expect(found.timeline.updatedAt.getTime()).toBe(
        article.timeline.updatedAt.getTime()
      );
    });
  });
});
