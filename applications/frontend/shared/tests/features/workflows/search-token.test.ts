/**
 * SearchToken Workflow Feature Test
 *
 * Firebase Emulatorを使用してSearchTokenワークフローの統合テストを行います。
 */

import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { createSearchByTokenWorkflow } from "@shared/workflows/search-token";
import {
  validateCriteria,
  SearchTokenType,
  ContentType,
  createSearchTokenIdentifier,
  searchTokenValueSchema,
  SearchToken,
  SearchReference,
  searchReferenceSchema,
  searchTokenSchema,
} from "@shared/domains/search-token";
import { generateNgrams } from "@shared/aspects/ngram";
import { FirebaseSearchTokenRepository } from "@shared/infrastructures/search-token";
import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import { FirebaseMemoRepository } from "@shared/infrastructures/memos";
import { FirebaseSeriesRepository } from "@shared/infrastructures/series";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../setup";
import {
  ArticleMold,
  ContentMold,
} from "../../support/molds/domains/article";
import { MemoMold } from "../../support/molds/domains/memo";
import { SeriesMold } from "../../support/molds/domains/series";
import { TagIdentifierMold } from "../../support/molds/domains/attributes/tag";
import { TimelineMold } from "../../support/molds/domains/common/date";
import type { Article, ArticleRepository } from "@shared/domains/articles";
import type { Memo, MemoRepository } from "@shared/domains/memo";
import type { Series, SeriesRepository } from "@shared/domains/series";
import type { SearchTokenRepository } from "@shared/domains/search-token";
import { PublishStatus } from "@shared/domains/common";

describe("Feature: SearchToken Workflow (実DB接続)", () => {
  let context: FeatureTestContext;
  let searchTokenRepository: SearchTokenRepository;
  let articleRepository: ArticleRepository;
  let memoRepository: MemoRepository;
  let seriesRepository: SeriesRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    context = await createFeatureTestContext();
    searchTokenRepository = FirebaseSearchTokenRepository(
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
    seriesRepository = FirebaseSeriesRepository(
      context.firestore,
      context.operations
    );
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  function createNgramSearchToken(
    text: string,
    references: SearchReference[]
  ): SearchToken[] {
    const ngrams = generateNgrams(text);
    const timeline = Forger(TimelineMold).forgeWithSeed(1);

    return ngrams.map((ngram) => {
      const value = searchTokenValueSchema.parse(ngram);
      const identifier = createSearchTokenIdentifier(SearchTokenType.NGRAM, value);

      return searchTokenSchema.parse({
        identifier,
        references,
        type: SearchTokenType.NGRAM,
        value,
        timeline,
      });
    });
  }

  function createTagSearchToken(
    tagIdentifier: string,
    references: SearchReference[]
  ): SearchToken {
    const value = searchTokenValueSchema.parse(tagIdentifier);
    const identifier = createSearchTokenIdentifier(SearchTokenType.TAG, value);
    const timeline = Forger(TimelineMold).forgeWithSeed(1);

    return searchTokenSchema.parse({
      identifier,
      references,
      type: SearchTokenType.TAG,
      value,
      timeline,
    });
  }

  function createSearchReference(
    type: typeof ContentType.ARTICLE | typeof ContentType.MEMO | typeof ContentType.SERIES,
    contentIdentifier: string
  ): SearchReference {
    return searchReferenceSchema.parse({
      identifier: {
        type,
        content: contentIdentifier,
      },
      score: 1.0,
      updatedAt: new Date(),
    });
  }

  function createSearchWorkflow() {
    return createSearchByTokenWorkflow(validateCriteria)(testLogger)(
      searchTokenRepository.ofIdentifiers
    )(articleRepository.ofIdentifiers)(memoRepository.ofIdentifiers)(
      seriesRepository.ofIdentifiers
    );
  }

  describe("N-gram検索ワークフロー", () => {
    it("N-gram検索でコンテンツが取得できる", async () => {
      const targetContent = Forger(ContentMold).forgeWithSeed(1, {
        value: "これはテスト記事です",
      });
      const article = Forger(ArticleMold).forgeWithSeed(1, {
        content: targetContent,
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(article).unwrap();

      const articleReference = createSearchReference(
        ContentType.ARTICLE,
        article.identifier
      );
      const searchTokens = createNgramSearchToken("テスト", [articleReference]);

      for (const token of searchTokens) {
        await searchTokenRepository.persist(token).unwrap();
      }

      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: "テスト",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
      }).unwrap();

      expect(results.length).toBe(1);
      expect((results[0] as Article).identifier).toBe(article.identifier);
    });
  });

  describe("タグ検索ワークフロー", () => {
    it("タグ検索でコンテンツが取得できる", async () => {
      const tagIdentifier = Forger(TagIdentifierMold).forgeWithSeed(100);
      const article = Forger(ArticleMold).forgeWithSeed(2, {
        tags: [tagIdentifier],
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(article).unwrap();

      const articleReference = createSearchReference(
        ContentType.ARTICLE,
        article.identifier
      );
      const tagToken = createTagSearchToken(tagIdentifier, [articleReference]);

      await searchTokenRepository.persist(tagToken).unwrap();

      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: null,
          tags: [tagIdentifier],
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
      }).unwrap();

      expect(results.length).toBe(1);
      expect((results[0] as Article).identifier).toBe(article.identifier);
    });
  });

  describe("ContentTypeフィルタリング", () => {
    it("ContentTypeでフィルタリングできる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(3, {
        status: PublishStatus.PUBLISHED,
      });
      const memo = Forger(MemoMold).forgeWithSeed(4, {
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(article).unwrap();
      await memoRepository.persist(memo).unwrap();

      const articleReference = createSearchReference(
        ContentType.ARTICLE,
        article.identifier
      );
      const memoReference = createSearchReference(
        ContentType.MEMO,
        memo.identifier
      );
      const searchTokens = createNgramSearchToken("共通キーワード", [
        articleReference,
        memoReference,
      ]);

      for (const token of searchTokens) {
        await searchTokenRepository.persist(token).unwrap();
      }

      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: "共通キーワード",
          tags: null,
          type: "article",
          sortBy: null,
          order: null,
          limit: null,
        },
      }).unwrap();

      expect(results.length).toBe(1);
      expect((results[0] as Article).identifier).toBe(article.identifier);
    });
  });

  describe("検索結果が空の場合", () => {
    it("検索条件がすべてnullの場合は空の配列を返す", async () => {
      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: null,
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
      }).unwrap();

      expect(results.length).toBe(0);
    });

    it("存在しないキーワードで検索すると空の配列を返す", async () => {
      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: "存在しないキーワード",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
      }).unwrap();

      expect(results.length).toBe(0);
    });
  });

  describe("バリデーションエラー", () => {
    it("無効な検索条件でValidationErrorが返る", async () => {
      const searchWorkflow = createSearchWorkflow();

      const result = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: null,
          tags: null,
          type: "invalid-type",
          sortBy: null,
          order: null,
          limit: null,
        },
      }).match({
        ok: () => ({ valid: true }),
        err: () => ({ valid: false }),
      });

      expect(result.valid).toBe(false);
    });

    it("空文字のfreeWordでValidationErrorが返る", async () => {
      const searchWorkflow = createSearchWorkflow();

      const result = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: "",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
      }).match({
        ok: () => ({ valid: true }),
        err: () => ({ valid: false }),
      });

      expect(result.valid).toBe(false);
    });
  });

  describe("複合検索", () => {
    it("N-gramとタグの複合検索でコンテンツが取得できる", async () => {
      const tagIdentifier = Forger(TagIdentifierMold).forgeWithSeed(200);
      const targetContent = Forger(ContentMold).forgeWithSeed(5, {
        value: "複合検索テスト記事",
      });
      const article = Forger(ArticleMold).forgeWithSeed(5, {
        content: targetContent,
        tags: [tagIdentifier],
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(article).unwrap();

      const articleReference = createSearchReference(
        ContentType.ARTICLE,
        article.identifier
      );

      const ngramTokens = createNgramSearchToken("複合検索", [articleReference]);
      for (const token of ngramTokens) {
        await searchTokenRepository.persist(token).unwrap();
      }

      const tagToken = createTagSearchToken(tagIdentifier, [articleReference]);
      await searchTokenRepository.persist(tagToken).unwrap();

      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: "複合検索",
          tags: [tagIdentifier],
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
      }).unwrap();

      expect(results.length).toBe(1);
      expect((results[0] as Article).identifier).toBe(article.identifier);
    });
  });

  describe("複数コンテンツタイプの検索", () => {
    it("異なるコンテンツタイプの結果を取得できる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(10, {
        status: PublishStatus.PUBLISHED,
      });
      const memo = Forger(MemoMold).forgeWithSeed(11, {
        status: PublishStatus.PUBLISHED,
      });
      const series = Forger(SeriesMold).forgeWithSeed(12);

      await articleRepository.persist(article).unwrap();
      await memoRepository.persist(memo).unwrap();
      await seriesRepository.persist(series).unwrap();

      const references = [
        createSearchReference(ContentType.ARTICLE, article.identifier),
        createSearchReference(ContentType.MEMO, memo.identifier),
        createSearchReference(ContentType.SERIES, series.identifier),
      ];

      const searchTokens = createNgramSearchToken("マルチタイプ", references);
      for (const token of searchTokens) {
        await searchTokenRepository.persist(token).unwrap();
      }

      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: "マルチタイプ",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
          limit: null,
        },
      }).unwrap();

      expect(results.length).toBe(3);

      const identifiers = results.map((result) => {
        if ("content" in result) return (result as Article).identifier;
        if ("entries" in result) return (result as Memo).identifier;
        return (result as Series).identifier;
      });

      expect(identifiers).toContain(article.identifier);
      expect(identifiers).toContain(memo.identifier);
      expect(identifiers).toContain(series.identifier);
    });
  });
});
