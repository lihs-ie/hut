/**
 * Article Actions Feature Test
 *
 * Firebase Emulatorを使用して記事アクションの統合テストを行います。
 * Server Actionは`revalidateTag`などNext.js固有機能を使用するため、
 * ワークフローのコアロジックを直接テストします。
 */

import { describe, it, expect, beforeEach, afterAll } from "vitest";
import {
  createAdminFeatureTestContext,
  cleanupAdminFeatureTest,
  testLogger,
} from "../setup";
import {
  Article,
  ArticleSnapshot,
  toSnapshot,
  UnvalidatedArticle,
  UnvalidatedCriteria,
  validateArticle,
  validateArticleIdentifier,
  validateCriteria,
} from "@shared/domains/articles";
import { PublishStatus, slugSchema } from "@shared/domains/common";
import {
  createArticleCreateWorkflow,
  createArticleEditWorkflow,
  createArticleSearchWorkflow,
  createArticleTerminateWorkflow,
  createArticleFindWorkflow,
} from "@shared/workflows/article";
import {
  ArticleMold,
  ArticleProperties,
} from "@shared-tests/support/molds/domains/article";
import {
  isValidationError,
  isAggregateNotFoundError,
} from "@shared/aspects/error";
import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import {
  getTestFirestoreInstance,
  createFirestoreOperations,
  cleanupTestApp,
} from "../firebase-test-utils";
import type { Firestore } from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { Forger } from "@lihs-ie/forger-ts";

const TEST_APP_NAME = "article-feature-test-app";

type SuccessResult<T> = { success: true; data: T };
type FailureResult<E> = { success: false; error: E };
type ResultOutcome<T, E> = SuccessResult<T> | FailureResult<E>;

function articleToUnvalidated(article: Article): UnvalidatedArticle {
  return {
    identifier: article.identifier,
    title: article.title,
    content: article.content,
    excerpt: article.excerpt,
    slug: article.slug,
    status: article.status,
    tags: [...article.tags],
    images: [...article.images],
    timeline: {
      createdAt: article.timeline.createdAt,
      updatedAt: article.timeline.updatedAt,
    },
  };
}

describe("Feature: Article Actions (実DB接続)", () => {
  let firestore: Firestore;
  let operations: FirestoreOperations;
  let articleRepository: ReturnType<typeof FirebaseArticleRepository>;

  const buildArticle = (overrides?: Partial<ArticleProperties>) =>
    Forger(ArticleMold).forge(overrides);

  beforeEach(async () => {
    await createAdminFeatureTestContext();
    firestore = getTestFirestoreInstance(TEST_APP_NAME);
    operations = createFirestoreOperations();
    articleRepository = FirebaseArticleRepository(firestore, operations);
  }, 30000);

  afterAll(async () => {
    await cleanupAdminFeatureTest();
    await cleanupTestApp(TEST_APP_NAME);
  }, 30000);

  describe("記事作成ワークフロー", () => {
    it("記事を作成し永続化されることを確認", async () => {
      const article = buildArticle({
        status: PublishStatus.PUBLISHED,
      });

      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        articleRepository.persist,
      )(testLogger);

      const createResult = await createWorkflow({
        payload: articleToUnvalidated(article),
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(createResult.success).toBe(true);

      const findWorkflow = createArticleFindWorkflow(validateArticleIdentifier)(
        articleRepository.find,
      )(testLogger);

      const findResult = await findWorkflow({
        payload: { identifier: article.identifier },
        now: new Date(),
      }).match<ResultOutcome<Article, unknown>>({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findResult.success).toBe(true);
      if (findResult.success) {
        expect(findResult.data.identifier).toBe(article.identifier);
        expect(findResult.data.title).toBe(article.title);
        expect(findResult.data.content).toBe(article.content);
        expect(findResult.data.slug).toBe(article.slug);
        expect(findResult.data.status).toBe(article.status);
      }
    });
  });

  describe("記事編集ワークフロー", () => {
    it("記事を編集し変更が反映されることを確認", async () => {
      const article = buildArticle({
        status: PublishStatus.DRAFT,
      });
      const unvalidatedArticle = articleToUnvalidated(article);

      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        articleRepository.persist,
      )(testLogger);

      await createWorkflow({
        payload: unvalidatedArticle,
        now: new Date(),
      }).unwrap();

      const findWorkflow = createArticleFindWorkflow(validateArticleIdentifier)(
        articleRepository.find,
      )(testLogger);

      const findBeforeEditResult = await findWorkflow({
        payload: { identifier: article.identifier },
        now: new Date(),
      }).match<ResultOutcome<Article, unknown>>({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findBeforeEditResult.success).toBe(true);

      const originalSnapshot: ArticleSnapshot = toSnapshot(article);
      const updatedTitle = "Updated Title - " + Date.now();
      const updatedAt = new Date(article.timeline.createdAt);
      updatedAt.setDate(updatedAt.getDate() + 10);

      const updatedUnvalidatedArticle: UnvalidatedArticle = {
        ...unvalidatedArticle,
        title: updatedTitle,
        status: PublishStatus.PUBLISHED,
        timeline: {
          createdAt: article.timeline.createdAt,
          updatedAt: updatedAt,
        },
      };

      const editWorkflow = createArticleEditWorkflow(validateArticle)(
        articleRepository.persist,
      )(testLogger);

      const editResult = await editWorkflow({
        payload: {
          unvalidated: updatedUnvalidatedArticle,
          before: originalSnapshot,
        },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(editResult.success).toBe(true);

      const findAfterEditResult = await findWorkflow({
        payload: { identifier: article.identifier },
        now: new Date(),
      }).match<ResultOutcome<Article, unknown>>({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findAfterEditResult.success).toBe(true);
      if (findAfterEditResult.success) {
        expect(findAfterEditResult.data.title).toBe(updatedTitle);
        expect(findAfterEditResult.data.status).toBe(PublishStatus.PUBLISHED);
      }
    });
  });

  describe("記事削除ワークフロー", () => {
    it("記事を削除し取得できなくなることを確認", async () => {
      const article = buildArticle();

      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        articleRepository.persist,
      )(testLogger);

      await createWorkflow({
        payload: articleToUnvalidated(article),
        now: new Date(),
      }).unwrap();

      const terminateWorkflow = createArticleTerminateWorkflow(
        validateArticleIdentifier,
      )(articleRepository.terminate)(testLogger);

      const terminateResult = await terminateWorkflow({
        payload: { identifier: article.identifier },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(terminateResult.success).toBe(true);

      const findWorkflow = createArticleFindWorkflow(validateArticleIdentifier)(
        articleRepository.find,
      )(testLogger);

      const findResult = await findWorkflow({
        payload: { identifier: article.identifier },
        now: new Date(),
      }).match<ResultOutcome<Article, unknown>>({
        ok: (found) => ({ success: true, data: found }),
        err: (error) => ({ success: false, error }),
      });

      expect(findResult.success).toBe(false);
      if (!findResult.success) {
        expect(isAggregateNotFoundError(findResult.error)).toBe(true);
      }
    });
  });

  describe("記事検索ワークフロー", () => {
    it("記事を検索条件で取得できることを確認", async () => {
      const uniqueSlug = slugSchema.parse(`test-slug-${Date.now()}`);
      const article = buildArticle({
        slug: uniqueSlug,
        status: PublishStatus.PUBLISHED,
      });

      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        articleRepository.persist,
      )(testLogger);

      await createWorkflow({
        payload: articleToUnvalidated(article),
        now: new Date(),
      }).unwrap();

      const searchWorkflow = createArticleSearchWorkflow(validateCriteria)(
        articleRepository.search,
      )(testLogger);

      const criteria: UnvalidatedCriteria = {
        status: PublishStatus.PUBLISHED,
      };

      const searchResult = await searchWorkflow({
        payload: criteria,
        now: new Date(),
      }).match<ResultOutcome<Article[], unknown>>({
        ok: (articles) => ({ success: true, data: articles }),
        err: (error) => ({ success: false, error }),
      });

      expect(searchResult.success).toBe(true);
      if (searchResult.success) {
        expect(searchResult.data.length).toBeGreaterThan(0);
        const found = searchResult.data.find(
          (articleItem) => articleItem.identifier === article.identifier,
        );
        expect(found).toBeDefined();
        expect(found?.slug).toBe(uniqueSlug);
      }
    });

    it("slugで記事を検索できることを確認", async () => {
      const uniqueSlug = slugSchema.parse(`unique-search-slug-${Date.now()}`);
      const article = buildArticle({
        slug: uniqueSlug,
        status: PublishStatus.PUBLISHED,
      });

      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        articleRepository.persist,
      )(testLogger);

      await createWorkflow({
        payload: articleToUnvalidated(article),
        now: new Date(),
      }).unwrap();

      const searchWorkflow = createArticleSearchWorkflow(validateCriteria)(
        articleRepository.search,
      )(testLogger);

      const criteria: UnvalidatedCriteria = {
        slug: uniqueSlug,
      };

      const searchResult = await searchWorkflow({
        payload: criteria,
        now: new Date(),
      }).match<ResultOutcome<Article[], unknown>>({
        ok: (articles) => ({ success: true, data: articles }),
        err: (error) => ({ success: false, error }),
      });

      expect(searchResult.success).toBe(true);
      if (searchResult.success) {
        expect(searchResult.data.length).toBe(1);
        expect(searchResult.data[0].slug).toBe(uniqueSlug);
      }
    });
  });

  describe("バリデーションエラー", () => {
    it("無効なデータでValidationErrorが返る", async () => {
      const invalidUnvalidatedArticle: UnvalidatedArticle = {
        identifier: "invalid-ulid",
        title: "",
        content: "",
        excerpt: "valid excerpt",
        slug: "valid-slug",
        status: "published",
        tags: [],
        images: [],
        timeline: {
          createdAt: new Date(),
          updatedAt: new Date(),
        },
      };

      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        articleRepository.persist,
      )(testLogger);

      const result = await createWorkflow({
        payload: invalidUnvalidatedArticle,
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (errors) => ({ success: false, errors }),
      });

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
        if (Array.isArray(result.errors)) {
          expect(result.errors.length).toBeGreaterThan(0);
          expect(result.errors.every((error) => isValidationError(error))).toBe(
            true,
          );
        }
      }
    });

    it("空のタイトルでValidationErrorが返る", async () => {
      const article = buildArticle();
      const unvalidated = articleToUnvalidated(article);

      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        articleRepository.persist,
      )(testLogger);

      const result = await createWorkflow({
        payload: { ...unvalidated, title: "" },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (errors) => ({ success: false, errors }),
      });

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
      }
    });

    it("空のコンテンツでValidationErrorが返る", async () => {
      const article = buildArticle();
      const unvalidated = articleToUnvalidated(article);

      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        articleRepository.persist,
      )(testLogger);

      const result = await createWorkflow({
        payload: { ...unvalidated, content: "" },
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (errors) => ({ success: false, errors }),
      });

      expect(result.success).toBe(false);
      if (!result.success && "errors" in result) {
        expect(Array.isArray(result.errors)).toBe(true);
      }
    });
  });

  describe("重複エラー", () => {
    it("同じslugの記事を作成するとエラーが返る", async () => {
      const sharedSlug = slugSchema.parse(`duplicate-slug-${Date.now()}`);

      const article1 = buildArticle({ slug: sharedSlug });
      const article2 = buildArticle({ slug: sharedSlug });

      const createWorkflow = createArticleCreateWorkflow(validateArticle)(
        articleRepository.persist,
      )(testLogger);

      const result1 = await createWorkflow({
        payload: articleToUnvalidated(article1),
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(result1.success).toBe(true);

      const result2 = await createWorkflow({
        payload: articleToUnvalidated(article2),
        now: new Date(),
      }).match({
        ok: () => ({ success: true }),
        err: (error) => ({ success: false, error }),
      });

      expect(result2.success).toBe(false);
    });
  });
});
