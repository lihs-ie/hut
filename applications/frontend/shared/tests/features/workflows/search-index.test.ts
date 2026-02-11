/**
 * SearchIndex Workflow Feature Test
 *
 * Firebase Emulatorを使用してSearchIndex Workflowの統合テストを行います。
 */

import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import { createSearchByIndexWorkflow } from "@shared/workflows/search-index";
import {
  validateCriteria,
  ContentType,
  SearchIndex,
} from "@shared/domains/search-index";
import { generateNgrams, score } from "@shared/aspects/ngram";
import { FirebaseSearchIndexRepository } from "@shared/infrastructures/search-index";
import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import { FirebaseMemoRepository } from "@shared/infrastructures/memos";
import { FirebaseSeriesRepository } from "@shared/infrastructures/series";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  testLogger,
  type FeatureTestContext,
} from "../setup";
import { ArticleMold } from "../../support/molds/domains/article";
import { MemoMold } from "../../support/molds/domains/memo";
import { SeriesMold } from "../../support/molds/domains/series";
import { TagIdentifierMold } from "../../support/molds/domains/attributes/tag";
import {
  SearchIndexMold,
  SearchIndexTitleMold,
  SearchIndexExcerptMold,
} from "../../support/molds/domains/search-index";
import type { Article, ArticleRepository } from "@shared/domains/articles";
import type { Memo, MemoRepository } from "@shared/domains/memo";
import type { Series, SeriesRepository } from "@shared/domains/series";
import type { SearchIndexRepository } from "@shared/domains/search-index";
import { PublishStatus } from "@shared/domains/common";
import { setDoc, doc, collection, type Firestore } from "firebase/firestore";

describe("Feature: SearchIndex Workflow (実DB接続)", () => {
  let context: FeatureTestContext;
  let searchIndexRepository: SearchIndexRepository;
  let articleRepository: ArticleRepository;
  let memoRepository: MemoRepository;
  let seriesRepository: SeriesRepository;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    context = await createFeatureTestContext();
    searchIndexRepository = FirebaseSearchIndexRepository(
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

  async function persistSearchIndex(
    firestore: Firestore,
    searchIndex: SearchIndex
  ): Promise<void> {
    const searchIndexCollection = collection(firestore, "search-index");
    const documentReference = doc(searchIndexCollection, searchIndex.identifier);

    const persistedData = {
      identifier: searchIndex.identifier,
      title: searchIndex.title,
      excerpt: searchIndex.excerpt,
      tags: searchIndex.tags,
      type: searchIndex.type,
      reference: searchIndex.reference,
      timeline: {
        createdAt: searchIndex.timeline.createdAt.toISOString(),
        updatedAt: searchIndex.timeline.updatedAt.toISOString(),
      },
      ngrams: generateNgrams(
        [searchIndex.title, searchIndex.excerpt, ...searchIndex.tags].join(" ")
      ),
      version: 1,
    };

    await setDoc(documentReference, persistedData);
  }

  function createTestScorer() {
    return score<"title" | "excerpt" | "tags">(() => 1);
  }

  function createSearchWorkflow() {
    return createSearchByIndexWorkflow(validateCriteria)(testLogger)(
      createTestScorer()
    )(searchIndexRepository.search)(articleRepository.ofIdentifiers)(
      memoRepository.ofIdentifiers
    )(seriesRepository.ofIdentifiers);
  }

  describe("フリーワード検索", () => {
    it("フリーワード検索でコンテンツが取得できる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(1, {
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(article).unwrap();

      const searchIndex = Forger(SearchIndexMold).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
        reference: article.identifier,
        title: Forger(SearchIndexTitleMold).forgeWithSeed(1, {
          value: "テスト記事タイトル",
        }),
        excerpt: Forger(SearchIndexExcerptMold).forgeWithSeed(1, {
          value: "これはテスト記事の抜粋です",
        }),
      });

      await persistSearchIndex(context.firestore, searchIndex);

      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: "テスト",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
        },
      }).unwrap();

      expect(results.length).toBe(1);
      expect((results[0] as Article).identifier).toBe(article.identifier);
    });
  });

  describe("タグ検索", () => {
    it("タグ検索でコンテンツが取得できる", async () => {
      const tagIdentifier = Forger(TagIdentifierMold).forgeWithSeed(100);

      const article = Forger(ArticleMold).forgeWithSeed(2, {
        tags: [tagIdentifier],
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(article).unwrap();

      const searchIndex = Forger(SearchIndexMold).forgeWithSeed(2, {
        type: ContentType.ARTICLE,
        reference: article.identifier,
        tags: [tagIdentifier],
      });

      await persistSearchIndex(context.firestore, searchIndex);

      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: null,
          tags: [tagIdentifier],
          type: null,
          sortBy: null,
          order: null,
        },
      }).unwrap();

      expect(results.length).toBe(1);
      expect((results[0] as Article).identifier).toBe(article.identifier);
    });
  });

  describe("複合検索", () => {
    it("フリーワードとタグの複合検索でコンテンツが取得できる", async () => {
      const tagIdentifier = Forger(TagIdentifierMold).forgeWithSeed(200);

      const article = Forger(ArticleMold).forgeWithSeed(3, {
        tags: [tagIdentifier],
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(article).unwrap();

      const searchIndex = Forger(SearchIndexMold).forgeWithSeed(3, {
        type: ContentType.ARTICLE,
        reference: article.identifier,
        title: Forger(SearchIndexTitleMold).forgeWithSeed(3, {
          value: "複合検索テスト",
        }),
        tags: [tagIdentifier],
      });

      await persistSearchIndex(context.firestore, searchIndex);

      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: "複合検索",
          tags: [tagIdentifier],
          type: null,
          sortBy: null,
          order: null,
        },
      }).unwrap();

      expect(results.length).toBe(1);
      expect((results[0] as Article).identifier).toBe(article.identifier);
    });
  });

  describe("ContentTypeフィルタリング", () => {
    it("ContentTypeでフィルタリングできる", async () => {
      const article = Forger(ArticleMold).forgeWithSeed(4, {
        status: PublishStatus.PUBLISHED,
      });
      const memo = Forger(MemoMold).forgeWithSeed(5, {
        status: PublishStatus.PUBLISHED,
      });

      await articleRepository.persist(article).unwrap();
      await memoRepository.persist(memo).unwrap();

      const articleSearchIndex = Forger(SearchIndexMold).forgeWithSeed(4, {
        type: ContentType.ARTICLE,
        reference: article.identifier,
        title: Forger(SearchIndexTitleMold).forgeWithSeed(4, {
          value: "共通キーワード記事",
        }),
      });

      const memoSearchIndex = Forger(SearchIndexMold).forgeWithSeed(5, {
        type: ContentType.MEMO,
        reference: memo.identifier,
        title: Forger(SearchIndexTitleMold).forgeWithSeed(5, {
          value: "共通キーワードメモ",
        }),
      });

      await persistSearchIndex(context.firestore, articleSearchIndex);
      await persistSearchIndex(context.firestore, memoSearchIndex);

      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: "共通キーワード",
          tags: null,
          type: "article",
          sortBy: null,
          order: null,
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
        },
      }).match({
        ok: () => ({ valid: true }),
        err: () => ({ valid: false }),
      });

      expect(result.valid).toBe(false);
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

      const articleSearchIndex = Forger(SearchIndexMold).forgeWithSeed(10, {
        type: ContentType.ARTICLE,
        reference: article.identifier,
        title: Forger(SearchIndexTitleMold).forgeWithSeed(10, {
          value: "マルチタイプ記事",
        }),
      });

      const memoSearchIndex = Forger(SearchIndexMold).forgeWithSeed(11, {
        type: ContentType.MEMO,
        reference: memo.identifier,
        title: Forger(SearchIndexTitleMold).forgeWithSeed(11, {
          value: "マルチタイプメモ",
        }),
      });

      const seriesSearchIndex = Forger(SearchIndexMold).forgeWithSeed(12, {
        type: ContentType.SERIES,
        reference: series.identifier,
        title: Forger(SearchIndexTitleMold).forgeWithSeed(12, {
          value: "マルチタイプシリーズ",
        }),
      });

      await persistSearchIndex(context.firestore, articleSearchIndex);
      await persistSearchIndex(context.firestore, memoSearchIndex);
      await persistSearchIndex(context.firestore, seriesSearchIndex);

      const searchWorkflow = createSearchWorkflow();

      const results = await searchWorkflow({
        now: new Date(),
        payload: {
          freeWord: "マルチタイプ",
          tags: null,
          type: null,
          sortBy: null,
          order: null,
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
