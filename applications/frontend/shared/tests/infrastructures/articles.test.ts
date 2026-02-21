import { describe, it, expect, beforeEach } from "vitest";
import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import {
  createTestFirestoreWithSeed,
  clearFirestore,
  type Firestore,
} from "../support/mock/firebase/firestore";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ArticleMold,
  ArticleIdentifierMold,
  SlugMold,
} from "../support/molds/domains/article/common";
import { PublishStatus } from "@shared/domains/common";
import { TagIdentifierMold } from "../support/molds/domains/attributes/tag";
import type { TagIdentifier } from "@shared/domains/attributes/tag";
import {
  isAggregateNotFoundError,
  isDuplicationError,
} from "@shared/aspects/error";
import {
  validateCriteria,
  type Criteria,
  type ArticleSlug,
} from "@shared/domains/articles";
import type { FirestoreOperations } from "@shared/infrastructures/common";

describe("infrastructures/articles", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnv = await createTestFirestoreWithSeed({});
    firestore = testEnv.firestore;
    operations = testEnv.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  const TestTags = {
    TYPESCRIPT: Forger(TagIdentifierMold).forgeWithSeed(1001),
    REACT: Forger(TagIdentifierMold).forgeWithSeed(1002),
    RUST: Forger(TagIdentifierMold).forgeWithSeed(1003),
  };

  const createCriteria = (params: {
    slug?: ArticleSlug | null;
    status?: PublishStatus | null;
    freeWord?: string | null;
    tags?: TagIdentifier[] | null;
  }): Criteria => {
    return validateCriteria({
      slug: params.slug ?? null,
      status: params.status ?? null,
      freeWord: params.freeWord ?? null,
      tags: params.tags ?? null,
    }).unwrap();
  };

  describe("FirebaseArticleRepository", () => {
    describe("persist", () => {
      it("新しい記事を保存できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article = Forger(ArticleMold).forgeWithSeed(1);

        const result = await repository.persist(article).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(article.identifier).unwrap();
        expect(found.identifier).toBe(article.identifier);
        expect(found.title).toBe(article.title);
      });

      it("既存の記事を更新できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article = Forger(ArticleMold).forgeWithSeed(2);

        await repository.persist(article).unwrap();

        const updatedArticle = Forger(ArticleMold).forgeWithSeed(0, {
          ...article,
          title: "Updated Title" as typeof article.title,
        });

        const result = await repository.persist(updatedArticle).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(article.identifier).unwrap();
        expect(found.title).toBe("Updated Title");
      });

      it("存在しない記事を更新しようとするとエラーになる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article = Forger(ArticleMold).forgeWithSeed(3);

        await repository.persist(article).unwrap();

        clearFirestore(firestore);

        const updatedArticle = Forger(ArticleMold).forgeWithSeed(0, {
          ...article,
          title: "Updated Title" as typeof article.title,
        });

        const result = await repository.persist(updatedArticle).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });

      it("重複する記事を作成しようとするとエラーになる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article = Forger(ArticleMold).forgeWithSeed(4);

        await repository.persist(article).unwrap();

        const repository2 = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const duplicateArticle = Forger(ArticleMold).forgeWithSeed(5, {
          identifier: article.identifier,
        });

        const result = await repository2.persist(duplicateArticle).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });
    });

    describe("find", () => {
      it("存在する記事を取得できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article = Forger(ArticleMold).forgeWithSeed(10);

        await repository.persist(article).unwrap();

        const found = await repository.find(article.identifier).unwrap();

        expect(found.identifier).toBe(article.identifier);
        expect(found.title).toBe(article.title);
        expect(found.content).toBe(article.content);
        expect(found.slug).toBe(article.slug);
        expect(found.status).toBe(article.status);
      });

      it("存在しない記事を取得しようとするとエラーになる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const identifier = Forger(ArticleIdentifierMold).forgeWithSeed(11);

        const result = await repository.find(identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("findBySlug", () => {
      it("slugで記事を取得できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const targetSlug = Forger(SlugMold).forgeWithSeed(12, {
          value: "find-by-slug-test",
        });
        const article = Forger(ArticleMold).forgeWithSeed(13, {
          slug: targetSlug,
        });

        await repository.persist(article).unwrap();

        const found = await repository.findBySlug(targetSlug).unwrap();

        expect(found.identifier).toBe(article.identifier);
        expect(found.slug).toBe(targetSlug);
      });

      it("存在しないslugで検索するとエラーになる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const nonExistentSlug = Forger(SlugMold).forgeWithSeed(14, {
          value: "non-existent-slug",
        });

        const result = await repository.findBySlug(nonExistentSlug).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("ofIdentifiers", () => {
      it("複数の記事を取得できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const articles = Forger(ArticleMold).forgeMultiWithSeed(3, 20);

        for (const article of articles) {
          await repository.persist(article).unwrap();
        }

        const identifiers = articles.map((a) => a.identifier);
        const found = await repository.ofIdentifiers(identifiers).unwrap();

        expect(found.length).toBe(3);
      });

      it("空の配列を渡すと空の配列を返す", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );

        const found = await repository.ofIdentifiers([]).unwrap();

        expect(found.length).toBe(0);
      });

      it("存在しない記事が含まれるとエラーになる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article = Forger(ArticleMold).forgeWithSeed(25);

        await repository.persist(article).unwrap();

        const nonExistentId = Forger(ArticleIdentifierMold).forgeWithSeed(26);
        const identifiers = [article.identifier, nonExistentId];

        const result = await repository.ofIdentifiers(identifiers).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("search", () => {
      it("全ての記事を取得できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const articles = Forger(ArticleMold).forgeMultiWithSeed(5, 30);

        for (const article of articles) {
          await repository.persist(article).unwrap();
        }

        const criteria = createCriteria({});
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(5);
      });

      it("slugで記事を検索できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const targetSlug = Forger(SlugMold).forgeWithSeed(40, {
          value: "target-slug",
        });
        const article1 = Forger(ArticleMold).forgeWithSeed(41, {
          slug: targetSlug,
        });
        const article2 = Forger(ArticleMold).forgeWithSeed(42);

        await repository.persist(article1).unwrap();
        await repository.persist(article2).unwrap();

        const criteria = createCriteria({ slug: targetSlug });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.slug).toBe(targetSlug);
      });

      it("statusで記事を検索できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article1 = Forger(ArticleMold).forgeWithSeed(50, {
          status: PublishStatus.PUBLISHED,
        });
        const article2 = Forger(ArticleMold).forgeWithSeed(51, {
          status: PublishStatus.DRAFT,
        });

        await repository.persist(article1).unwrap();
        await repository.persist(article2).unwrap();

        const criteria = createCriteria({ status: PublishStatus.PUBLISHED });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.status).toBe(PublishStatus.PUBLISHED);
      });

      it("tagsで記事を検索できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article1 = Forger(ArticleMold).forgeWithSeed(60, {
          tags: [TestTags.TYPESCRIPT, TestTags.REACT],
        });
        const article2 = Forger(ArticleMold).forgeWithSeed(61, {
          tags: [TestTags.RUST],
        });

        await repository.persist(article1).unwrap();
        await repository.persist(article2).unwrap();

        const criteria = createCriteria({ tags: [TestTags.TYPESCRIPT] });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
      });

      it("freeWordでタイトルを検索できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article1 = Forger(ArticleMold).forgeWithSeed(70, {
          title: "TypeScript入門" as typeof article1.title,
        });
        const article2 = Forger(ArticleMold).forgeWithSeed(71, {
          title: "Python入門" as typeof article2.title,
        });

        await repository.persist(article1).unwrap();
        await repository.persist(article2).unwrap();

        const criteria = createCriteria({ freeWord: "TypeScript" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.title).toBe("TypeScript入門");
      });

      it("freeWordでcontentを検索できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article1 = Forger(ArticleMold).forgeWithSeed(80, {
          content:
            "This article is about JavaScript" as typeof article1.content,
        });
        const article2 = Forger(ArticleMold).forgeWithSeed(81, {
          content: "This article is about Rust" as typeof article2.content,
        });

        await repository.persist(article1).unwrap();
        await repository.persist(article2).unwrap();

        const criteria = createCriteria({ freeWord: "JavaScript" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
      });

      it("freeWordでexcerptを検索できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article1 = Forger(ArticleMold).forgeWithSeed(90, {
          excerpt: "Learn about GraphQL" as typeof article1.excerpt,
        });
        const article2 = Forger(ArticleMold).forgeWithSeed(91, {
          excerpt: "Learn about REST" as typeof article2.excerpt,
        });

        await repository.persist(article1).unwrap();
        await repository.persist(article2).unwrap();

        const criteria = createCriteria({ freeWord: "GraphQL" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
      });

      it("複合条件で検索できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article1 = Forger(ArticleMold).forgeWithSeed(100, {
          status: PublishStatus.PUBLISHED,
          tags: [TestTags.TYPESCRIPT],
        });
        const article2 = Forger(ArticleMold).forgeWithSeed(101, {
          status: PublishStatus.DRAFT,
          tags: [TestTags.TYPESCRIPT],
        });
        const article3 = Forger(ArticleMold).forgeWithSeed(102, {
          status: PublishStatus.PUBLISHED,
          tags: [TestTags.RUST],
        });

        await repository.persist(article1).unwrap();
        await repository.persist(article2).unwrap();
        await repository.persist(article3).unwrap();

        const criteria = createCriteria({
          status: PublishStatus.PUBLISHED,
          tags: [TestTags.TYPESCRIPT],
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.identifier).toBe(article1.identifier);
      });
    });

    describe("slug uniqueness", () => {
      it("同じslugで新しい記事を作成しようとするとエラーになる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const slug = Forger(SlugMold).forgeWithSeed(200, {
          value: "duplicate-slug",
        });
        const article1 = Forger(ArticleMold).forgeWithSeed(201, { slug });
        const article2 = Forger(ArticleMold).forgeWithSeed(202, { slug });

        await repository.persist(article1).unwrap();

        const result = await repository.persist(article2).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });

      it("記事更新時に他の記事と同じslugに変更しようとするとエラーになる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const slug1 = Forger(SlugMold).forgeWithSeed(210, {
          value: "slug-one",
        });
        const slug2 = Forger(SlugMold).forgeWithSeed(211, {
          value: "slug-two",
        });

        const article1 = Forger(ArticleMold).forgeWithSeed(212, {
          slug: slug1,
        });
        const article2 = Forger(ArticleMold).forgeWithSeed(213, {
          slug: slug2,
        });

        await repository.persist(article1).unwrap();
        await repository.persist(article2).unwrap();

        const updatedArticle2 = Forger(ArticleMold).forgeWithSeed(0, {
          ...article2,
          slug: slug1,
        });

        const result = await repository.persist(updatedArticle2).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });

      it("記事削除後に同じslugで新しい記事を作成できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const slug = Forger(SlugMold).forgeWithSeed(220, {
          value: "reusable-slug",
        });
        const article1 = Forger(ArticleMold).forgeWithSeed(221, { slug });

        await repository.persist(article1).unwrap();
        await repository.terminate(article1.identifier).unwrap();

        const article2 = Forger(ArticleMold).forgeWithSeed(222, { slug });
        const result = await repository.persist(article2).unwrap();

        expect(result).toBeUndefined();
      });

      it("同じ記事のslugを変更できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const oldSlug = Forger(SlugMold).forgeWithSeed(230, {
          value: "old-slug",
        });
        const newSlug = Forger(SlugMold).forgeWithSeed(231, {
          value: "new-slug",
        });

        const article = Forger(ArticleMold).forgeWithSeed(232, {
          slug: oldSlug,
        });

        await repository.persist(article).unwrap();

        const updatedArticle = Forger(ArticleMold).forgeWithSeed(0, {
          ...article,
          slug: newSlug,
        });

        const result = await repository.persist(updatedArticle).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(article.identifier).unwrap();
        expect(found.slug).toBe(newSlug);
      });
    });

    describe("terminate", () => {
      it("記事を削除できる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const article = Forger(ArticleMold).forgeWithSeed(110);

        await repository.persist(article).unwrap();

        const result = await repository.terminate(article.identifier).unwrap();

        expect(result).toBeUndefined();

        const findResult = await repository.find(article.identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(findResult).not.toBeNull();
        expect(isAggregateNotFoundError(findResult)).toBe(true);
      });

      it("存在しない記事を削除しようとするとエラーになる", async () => {
        const repository = FirebaseArticleRepository(
          firestore,
          getOperations(),
        );
        const identifier = Forger(ArticleIdentifierMold).forgeWithSeed(111);

        const result = await repository.terminate(identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });
  });
});
