import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  articleIdentifierSchema,
  validateArticleIdentifier,
  titleSchema,
  contentSchema,
  excerptSchema,
  articleSchema,
  validateArticle,
  toDraft,
  toPublished,
  toSnapshot,
  criteriaSchema,
  validateCriteria,
} from "@shared/domains/articles";
import { PublishStatus } from "@shared/domains/common";
import {
  ArticleMold,
  ArticleIdentifierMold,
} from "../../support/molds/domains/article";
import { TimelineMold } from "../../support/molds/domains/common/date";
import { TagIdentifierMold } from "../../support/molds/domains/attributes/tag";
import { SlugMold } from "../../support/molds/domains/common/slug";
import {
  describeIdentifierSchema,
  describeStringLengthSchema,
} from "../../support/helpers";

describe("domains/articles/common", () => {
  describe("articleIdentifierSchema", () => {
    describeIdentifierSchema(
      "ArticleIdentifier",
      articleIdentifierSchema,
      () => Forger(ArticleIdentifierMold).forge(),
      (count) => Forger(ArticleIdentifierMold).forgeMulti(count),
    );
  });

  describe("validateArticleIdentifier", () => {
    it("有効なULIDでokを返す", () => {
      const identifier = Forger(ArticleIdentifierMold).forge();
      const result = validateArticleIdentifier(identifier);
      expect(result.isOk).toBe(true);
    });

    it("無効な文字列でerrを返す", () => {
      const result = validateArticleIdentifier("invalid");
      expect(result.isErr).toBe(true);
      if (result.isErr) {
        expect(result.unwrapError().field).toBe("ArticleIdentifier");
      }
    });
  });

  describe("titleSchema", () => {
    describeStringLengthSchema("タイトル", titleSchema, 1, 100, {
      minLengthMessage: "Title must be at least 1 character long",
      maxLengthMessage: "Title must be at most 100 characters long",
    });

    it("日本語を含むタイトルは有効", () => {
      const result = titleSchema.safeParse("テスト記事タイトル");
      expect(result.success).toBe(true);
    });
  });

  describe("contentSchema", () => {
    describe("有効なコンテンツの検証", () => {
      it("1文字は有効", () => {
        const result = contentSchema.safeParse("a");
        expect(result.success).toBe(true);
      });

      it("長いコンテンツも有効", () => {
        const result = contentSchema.safeParse("a".repeat(10000));
        expect(result.success).toBe(true);
      });
    });

    describe("無効なコンテンツの検証", () => {
      it("空文字列は無効", () => {
        const result = contentSchema.safeParse("");
        expect(result.success).toBe(false);
        if (!result.success) {
          expect(result.error.issues[0].message).toBe(
            "Content must be at least 1 character long",
          );
        }
      });
    });
  });

  describe("excerptSchema", () => {
    describeStringLengthSchema("抜粋", excerptSchema, 0, 300, {
      maxLengthMessage: "Excerpt must be at most 300 characters long",
      allowEmpty: true,
    });
  });

  describe("articleSchema", () => {
    describe("有効なArticleの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const article = Forger(ArticleMold).forge();
        const result = articleSchema.safeParse(article);
        expect(result.success).toBe(true);
      });

      it("タグが空配列でも有効", () => {
        const article = Forger(ArticleMold).forge({ tags: [] });
        const result = articleSchema.safeParse(article);
        expect(result.success).toBe(true);
      });
    });

    describe("無効なArticleの検証", () => {
      const createArticleWithOverrides = (
        overrides: Record<string, unknown>,
      ) => ({
        identifier: Forger(ArticleIdentifierMold).forge(),
        title: "Test",
        content: "Content",
        excerpt: "Excerpt",
        slug: "test-slug",
        status: "draft",
        tags: [],
        timeline: { createdAt: new Date(), updatedAt: new Date() },
        ...overrides,
      });

      it("identifierが無効な場合は無効", () => {
        const result = articleSchema.safeParse(
          createArticleWithOverrides({ identifier: "invalid" }),
        );
        expect(result.success).toBe(false);
      });

      it("titleが空の場合は無効", () => {
        const result = articleSchema.safeParse(
          createArticleWithOverrides({ title: "" }),
        );
        expect(result.success).toBe(false);
      });

      it("contentが空の場合は無効", () => {
        const result = articleSchema.safeParse(
          createArticleWithOverrides({ content: "" }),
        );
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateArticle", () => {
    it("有効なUnvalidatedArticleでokを返す", () => {
      const result = validateArticle({
        identifier: Forger(ArticleIdentifierMold).forge(),
        title: "テスト記事",
        content: "これはテスト記事の内容です",
        excerpt: "抜粋",
        slug: "test-article",
        status: "draft",
        tags: [],
        images: [],
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedArticleでerrを返す", () => {
      const result = validateArticle({
        identifier: "invalid",
        title: "",
        content: "",
        excerpt: "a".repeat(301),
        slug: "",
        status: "invalid",
        tags: [],
        images: [],
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("toDraft", () => {
    it("記事のステータスをDRAFTに変更し、他のフィールドは保持する", () => {
      const article = Forger(ArticleMold).forge({
        status: PublishStatus.PUBLISHED,
      });
      const draftArticle = toDraft(article);

      expect(draftArticle.status).toBe(PublishStatus.DRAFT);
      expect(draftArticle.identifier).toBe(article.identifier);
      expect(draftArticle.title).toBe(article.title);
      expect(draftArticle.content).toBe(article.content);
    });
  });

  describe("toPublished", () => {
    it("記事のステータスをPUBLISHEDに変更し、他のフィールドは保持する", () => {
      const article = Forger(ArticleMold).forge({
        status: PublishStatus.DRAFT,
      });
      const publishedArticle = toPublished(article);

      expect(publishedArticle.status).toBe(PublishStatus.PUBLISHED);
      expect(publishedArticle.identifier).toBe(article.identifier);
      expect(publishedArticle.title).toBe(article.title);
    });
  });

  describe("toSnapshot", () => {
    it("ArticleからArticleSnapshotを作成する", () => {
      const article = Forger(ArticleMold).forge();
      const snapshot = toSnapshot(article);

      expect(snapshot.identifier).toBe(article.identifier);
      expect(snapshot.title).toBe(article.title);
      expect(snapshot.status).toBe(article.status);
    });
  });

  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("全てnullで有効", () => {
        const result = criteriaSchema.safeParse({
          slug: null,
          status: null,
          freeWord: null,
          tags: null,
        });
        expect(result.success).toBe(true);
      });

      it("全てのフィールドを指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          slug: Forger(SlugMold).forge(),
          status: "published",
          freeWord: "検索ワード",
          tags: Forger(TagIdentifierMold).forgeMulti(2),
        });
        expect(result.success).toBe(true);
      });

      it("undefinedフィールドも有効", () => {
        const result = criteriaSchema.safeParse({});
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("freeWordが空文字列の場合は無効", () => {
        const result = criteriaSchema.safeParse({ freeWord: "" });
        expect(result.success).toBe(false);
      });

      it("freeWordが101文字以上の場合は無効", () => {
        const result = criteriaSchema.safeParse({ freeWord: "a".repeat(101) });
        expect(result.success).toBe(false);
      });

      it("無効なstatusの場合は無効", () => {
        const result = criteriaSchema.safeParse({ status: "invalid-status" });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateCriteria", () => {
    it("有効なCriteriaでokを返す", () => {
      const result = validateCriteria({
        slug: null,
        status: "published",
        freeWord: "テスト",
        tags: null,
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なCriteriaでerrを返す", () => {
      const result = validateCriteria({
        slug: null,
        status: "invalid",
        freeWord: null,
        tags: null,
      });
      expect(result.isErr).toBe(true);
    });
  });
});
