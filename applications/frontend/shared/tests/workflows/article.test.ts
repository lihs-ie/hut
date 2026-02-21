import { describe, it, expect, vi, beforeEach } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  createArticleFindWorkflow,
  createArticleFindBySlugWorkflow,
  createArticleSearchWorkflow,
  createArticleCreateWorkflow,
  createArticleTerminateWorkflow,
} from "@shared/workflows/article";
import {
  validateArticleIdentifier,
  validateArticle,
  validateCriteria,
} from "@shared/domains/articles";
import { validateSlug } from "@shared/domains/common/slug";
import { Logger, Environment } from "@shared/aspects/logger";
import { ok, err } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  isAggregateNotFoundError,
  unexpectedError,
} from "@shared/aspects/error";
import { PublishStatus } from "@shared/domains/common";
import {
  createPassthroughFilter,
  createPublishedOnlyFilter,
} from "@shared/workflows/common";
import {
  ArticleMold,
  ArticleIdentifierMold,
  SlugMold,
} from "../support/molds/domains/article";
import { Command } from "@shared/workflows/common";

describe("workflows/article", () => {
  const mockLogger = Logger(Environment.DEVELOPMENT);

  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
  });

  describe("createArticleFindWorkflow", () => {
    it("有効なidentifierで記事を取得できる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1);
      const findMock = vi.fn().mockReturnValue(ok(article).toAsync());

      const workflow = createArticleFindWorkflow(validateArticleIdentifier)(
        findMock
      )(mockLogger);

      const command: Command<{ identifier: string }> = {
        now: new Date(),
        payload: { identifier: article.identifier },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(article);
      expect(findMock).toHaveBeenCalledWith(article.identifier);
    });

    it("無効なidentifierでValidationErrorを返す", async () => {
      const findMock = vi.fn();

      const workflow = createArticleFindWorkflow(validateArticleIdentifier)(
        findMock
      )(mockLogger);

      const command: Command<{ identifier: string }> = {
        now: new Date(),
        payload: { identifier: "invalid-identifier" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(findMock).not.toHaveBeenCalled();
    });

    it("記事が見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const identifier = Forger(ArticleIdentifierMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError(
        "Article",
        "Article not found"
      );
      const findMock = vi.fn().mockReturnValue(err(notFoundError).toAsync());

      const workflow = createArticleFindWorkflow(validateArticleIdentifier)(
        findMock
      )(mockLogger);

      const command: Command<{ identifier: string }> = {
        now: new Date(),
        payload: { identifier },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(notFoundError);
    });
  });

  describe("createArticleFindBySlugWorkflow", () => {
    it("有効なslugで記事を取得できる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1);
      const findBySlugMock = vi.fn().mockReturnValue(ok(article).toAsync());

      const workflow = createArticleFindBySlugWorkflow(validateSlug)(
        mockLogger
      )(findBySlugMock)(createPassthroughFilter());

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: article.slug },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(article);
      expect(findBySlugMock).toHaveBeenCalledWith(article.slug);
    });

    it("無効なslugでValidationErrorを返す", async () => {
      const findBySlugMock = vi.fn();

      const workflow = createArticleFindBySlugWorkflow(validateSlug)(
        mockLogger
      )(findBySlugMock)(createPassthroughFilter());

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: "Invalid Slug With Spaces" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(findBySlugMock).not.toHaveBeenCalled();
    });

    it("記事が見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const slug = Forger(SlugMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError(
        "Article",
        "Article not found"
      );
      const findBySlugMock = vi
        .fn()
        .mockReturnValue(err(notFoundError).toAsync());

      const workflow = createArticleFindBySlugWorkflow(validateSlug)(
        mockLogger
      )(findBySlugMock)(createPassthroughFilter());

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(notFoundError);
    });

    it("createPublishedOnlyFilterでdraft状態の記事はAggregateNotFoundErrorを返す", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1, {
        status: PublishStatus.DRAFT,
      });
      const findBySlugMock = vi.fn().mockReturnValue(ok(article).toAsync());

      const workflow = createArticleFindBySlugWorkflow(validateSlug)(
        mockLogger
      )(findBySlugMock)(createPublishedOnlyFilter("Article"));

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: article.slug },
      };

      const error = await workflow(command).unwrapError();

      expect(isAggregateNotFoundError(error)).toBe(true);
    });

    it("createPublishedOnlyFilterでarchived状態の記事はAggregateNotFoundErrorを返す", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1, {
        status: PublishStatus.ARCHIVED,
      });
      const findBySlugMock = vi.fn().mockReturnValue(ok(article).toAsync());

      const workflow = createArticleFindBySlugWorkflow(validateSlug)(
        mockLogger
      )(findBySlugMock)(createPublishedOnlyFilter("Article"));

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: article.slug },
      };

      const error = await workflow(command).unwrapError();

      expect(isAggregateNotFoundError(error)).toBe(true);
    });

    it("createPublishedOnlyFilterでpublished状態の記事は正常に返す", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1, {
        status: PublishStatus.PUBLISHED,
      });
      const findBySlugMock = vi.fn().mockReturnValue(ok(article).toAsync());

      const workflow = createArticleFindBySlugWorkflow(validateSlug)(
        mockLogger
      )(findBySlugMock)(createPublishedOnlyFilter("Article"));

      const command: Command<{ slug: string }> = {
        now: new Date(),
        payload: { slug: article.slug },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(article);
    });
  });

  describe("createArticleSearchWorkflow", () => {
    it("有効な検索条件で記事を検索できる", async () => {
      const articles = Forger(ArticleMold).forgeMultiWithSeed(3, 1);
      const searchMock = vi.fn().mockReturnValue(ok(articles).toAsync());

      const workflow = createArticleSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const command: Command<{ status?: string }> = {
        now: new Date(),
        payload: { status: "published" },
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(articles);
      expect(searchMock).toHaveBeenCalled();
    });

    it("空の検索条件でも検索できる", async () => {
      const articles = Forger(ArticleMold).forgeMultiWithSeed(5, 1);
      const searchMock = vi.fn().mockReturnValue(ok(articles).toAsync());

      const workflow = createArticleSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const command: Command<Record<string, unknown>> = {
        now: new Date(),
        payload: {},
      };

      const result = await workflow(command).unwrap();

      expect(result).toEqual(articles);
    });

    it("検索でエラーが発生した場合はUnexpectedErrorを返す", async () => {
      const error = unexpectedError("Search failed");
      const searchMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createArticleSearchWorkflow(validateCriteria)(searchMock)(
        mockLogger
      );

      const command: Command<Record<string, unknown>> = {
        now: new Date(),
        payload: {},
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createArticleCreateWorkflow", () => {
    it("有効な記事データで記事を作成できる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1);
      const persistMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createArticleCreateWorkflow(validateArticle)(persistMock)(
        mockLogger
      );

      const command: Command<{
        identifier: string;
        title: string;
        content: string;
        excerpt: string;
        slug: string;
        status: string;
        tags: string[];
        images: string[];
        timeline: { createdAt: Date; updatedAt: Date };
      }> = {
        now: new Date(),
        payload: {
          identifier: article.identifier,
          title: article.title,
          content: article.content,
          excerpt: article.excerpt,
          slug: article.slug,
          status: article.status,
          tags: article.tags,
          images: article.images,
          timeline: article.timeline,
        },
      };

      const result = await workflow(command).unwrap();

      expect(result.payload.snapshot.identifier).toBe(article.identifier);
      expect(result.occurredAt).toBeDefined();
      expect(persistMock).toHaveBeenCalled();
    });

    it("無効な記事データでValidationErrorを返す", async () => {
      const persistMock = vi.fn();

      const workflow = createArticleCreateWorkflow(validateArticle)(persistMock)(
        mockLogger
      );

      const command: Command<{
        identifier: string;
        title: string;
        content: string;
        excerpt: string;
        slug: string;
        status: string;
        tags: string[];
        timeline: { createdAt: Date; updatedAt: Date };
      }> = {
        now: new Date(),
        payload: {
          identifier: "invalid",
          title: "", // 空のタイトルは無効
          content: "",
          excerpt: "",
          slug: "test-slug",
          status: "published",
          tags: [],
          timeline: { createdAt: new Date(), updatedAt: new Date() },
        },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(persistMock).not.toHaveBeenCalled();
    });

    it("永続化でエラーが発生した場合はエラーを返す", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1);
      const error = unexpectedError("Persist failed");
      const persistMock = vi.fn().mockReturnValue(err(error).toAsync());

      const workflow = createArticleCreateWorkflow(validateArticle)(persistMock)(
        mockLogger
      );

      const command: Command<{
        identifier: string;
        title: string;
        content: string;
        excerpt: string;
        slug: string;
        status: string;
        tags: string[];
        images: string[];
        timeline: { createdAt: Date; updatedAt: Date };
      }> = {
        now: new Date(),
        payload: {
          identifier: article.identifier,
          title: article.title,
          content: article.content,
          excerpt: article.excerpt,
          slug: article.slug,
          status: article.status,
          tags: article.tags,
          images: article.images,
          timeline: article.timeline,
        },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(error);
    });
  });

  describe("createArticleTerminateWorkflow", () => {
    it("有効なidentifierで記事を削除できる", async () => {
      const identifier = Forger(ArticleIdentifierMold).forgeWithSeed(1);
      const terminateMock = vi.fn().mockReturnValue(ok(undefined).toAsync());

      const workflow = createArticleTerminateWorkflow(validateArticleIdentifier)(
        terminateMock
      )(mockLogger);

      const command: Command<{ identifier: string }> = {
        now: new Date(),
        payload: { identifier },
      };

      const result = await workflow(command).unwrap();

      expect(result.payload.article).toBe(identifier);
      expect(result.occurredAt).toBeDefined();
      expect(terminateMock).toHaveBeenCalledWith(identifier);
    });

    it("無効なidentifierでValidationErrorを返す", async () => {
      const terminateMock = vi.fn();

      const workflow = createArticleTerminateWorkflow(validateArticleIdentifier)(
        terminateMock
      )(mockLogger);

      const command: Command<{ identifier: string }> = {
        now: new Date(),
        payload: { identifier: "invalid-identifier" },
      };

      const result = workflow(command);

      expect(await result.match({ ok: () => false, err: () => true })).toBe(
        true
      );
      expect(terminateMock).not.toHaveBeenCalled();
    });

    it("記事が見つからない場合はAggregateNotFoundErrorを返す", async () => {
      const identifier = Forger(ArticleIdentifierMold).forgeWithSeed(1);
      const notFoundError = aggregateNotFoundError(
        "Article",
        "Article not found"
      );
      const terminateMock = vi
        .fn()
        .mockReturnValue(err(notFoundError).toAsync());

      const workflow = createArticleTerminateWorkflow(validateArticleIdentifier)(
        terminateMock
      )(mockLogger);

      const command: Command<{ identifier: string }> = {
        now: new Date(),
        payload: { identifier },
      };

      const result = await workflow(command).unwrapError();

      expect(result).toEqual(notFoundError);
    });
  });
});
