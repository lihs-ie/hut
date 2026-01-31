import { describe, it, expect, beforeEach } from "vitest";
import { FirebaseSearchIndexRepository } from "@shared/infrastructures/search-index";
import {
  createTestFirestoreWithSeed,
  type Firestore,
} from "../support/mock/firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import {
  ContentType,
  Sort,
  Order,
  validateSearchIndex,
  type SearchIndex,
  type Criteria,
} from "@shared/domains/search-index/common";
import { ulid } from "ulid";
import { Tag } from "@shared/domains/common";

describe("infrastructures/search-index", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnv = await createTestFirestoreWithSeed({});
    firestore = testEnv.firestore;
    operations = testEnv.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  const createSearchIndex = (
    seed: number,
    overrides?: Partial<{
      title: string;
      excerpt: string;
      tags: string[];
      type: typeof ContentType.ARTICLE | typeof ContentType.MEMO | typeof ContentType.SERIES;
    }>
  ): SearchIndex => {
    const now = new Date();
    const identifier = ulid(now.getTime() + seed);
    const type = overrides?.type ?? ContentType.ARTICLE;

    return validateSearchIndex({
      identifier,
      title: overrides?.title ?? `テスト記事タイトル${seed}`,
      excerpt: overrides?.excerpt ?? `これはテスト記事${seed}の抜粋です。`,
      tags: overrides?.tags ?? [Tag.TYPESCRIPT],
      type,
      reference: identifier,
      timeline: {
        createdAt: now,
        updatedAt: now,
      },
    }).unwrap();
  };

  const createCriteria = (params: {
    freeWord?: string | null;
    tags?: string[] | null;
    type?: typeof ContentType.ALL | typeof ContentType.ARTICLE | typeof ContentType.MEMO | typeof ContentType.SERIES | null;
    sortBy?: typeof Sort.LATEST | typeof Sort.NEWEST | typeof Sort.OLDEST | null;
    order?: typeof Order.ASC | typeof Order.DESC | null;
  }): Criteria => {
    return {
      freeWord: params.freeWord ?? null,
      tags: params.tags ?? null,
      type: params.type ?? null,
      sortBy: params.sortBy ?? null,
      order: params.order ?? null,
    } as Criteria;
  };

  // Helper to persist search index directly to firestore mock
  const persistSearchIndex = async (
    firestore: Firestore,
    operations: FirestoreOperations,
    index: SearchIndex
  ) => {
    const collection = operations.collection(firestore, "search-index");
    const document = operations.doc(collection, index.identifier);

    await operations.setDoc(document, {
      identifier: index.identifier,
      title: index.title,
      excerpt: index.excerpt,
      tags: index.tags,
      type: index.type,
      reference: index.reference,
      timeline: {
        createdAt: index.timeline.createdAt.toISOString(),
        updatedAt: index.timeline.updatedAt.toISOString(),
      },
      ngrams: generateNgrams([index.title, index.excerpt, ...index.tags].join(" ")),
      version: 1,
    });
  };

  // Simple ngram generator for testing
  const generateNgrams = (text: string, min = 2, max = 4): string[] => {
    const normalized = text
      .toLowerCase()
      .replace(/\s+/g, "")
      .replace(/[^\p{L}\p{N}]/gu, "");
    const ngrams: Set<string> = new Set();

    for (let n = min; n <= max; n++) {
      for (let i = 0; i <= normalized.length - n; i++) {
        ngrams.add(normalized.slice(i, i + n));
      }
    }

    return Array.from(ngrams);
  };

  describe("FirebaseSearchIndexRepository", () => {
    describe("search", () => {
      it("全ての検索インデックスを取得できる", async () => {
        const ops = getOperations();
        const index1 = createSearchIndex(1);
        const index2 = createSearchIndex(2);

        await persistSearchIndex(firestore, ops, index1);
        await persistSearchIndex(firestore, ops, index2);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({});
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("freeWordでタイトルを検索できる", async () => {
        const ops = getOperations();
        // Use unique keywords with different tags to avoid ngram overlap
        const index1 = createSearchIndex(10, {
          title: "ZZZAlphaZZZ unique keyword",
          tags: [Tag.RUST],
          excerpt: "記事Aの抜粋",
        });
        const index2 = createSearchIndex(11, {
          title: "YYYBetaYYY different content",
          tags: [Tag.REACT],
          excerpt: "記事Bの抜粋",
        });

        await persistSearchIndex(firestore, ops, index1);
        await persistSearchIndex(firestore, ops, index2);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({ freeWord: "Alpha" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.title).toContain("Alpha");
      });

      it("freeWordで抜粋を検索できる", async () => {
        const ops = getOperations();
        const index1 = createSearchIndex(20, {
          title: "記事AAA",
          excerpt: "ZZZGammaZZZ unique content here",
          tags: [Tag.RUST],
        });
        const index2 = createSearchIndex(21, {
          title: "記事BBB",
          excerpt: "YYYDeltaYYY different content here",
          tags: [Tag.REACT],
        });

        await persistSearchIndex(firestore, ops, index1);
        await persistSearchIndex(firestore, ops, index2);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({ freeWord: "Gamma" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.excerpt).toContain("Gamma");
      });

      it("typeで記事を検索できる", async () => {
        const ops = getOperations();
        const articleIndex = createSearchIndex(30, { type: ContentType.ARTICLE });
        const memoIndex = createSearchIndex(31, { type: ContentType.MEMO });

        await persistSearchIndex(firestore, ops, articleIndex);
        await persistSearchIndex(firestore, ops, memoIndex);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({ type: ContentType.ARTICLE });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.type).toBe(ContentType.ARTICLE);
      });

      it("typeでメモを検索できる", async () => {
        const ops = getOperations();
        const articleIndex = createSearchIndex(40, { type: ContentType.ARTICLE });
        const memoIndex = createSearchIndex(41, { type: ContentType.MEMO });

        await persistSearchIndex(firestore, ops, articleIndex);
        await persistSearchIndex(firestore, ops, memoIndex);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({ type: ContentType.MEMO });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.type).toBe(ContentType.MEMO);
      });

      it("tagsで検索できる", async () => {
        const ops = getOperations();
        const index1 = createSearchIndex(50, { tags: [Tag.TYPESCRIPT, Tag.REACT] });
        const index2 = createSearchIndex(51, { tags: [Tag.RUST] });

        await persistSearchIndex(firestore, ops, index1);
        await persistSearchIndex(firestore, ops, index2);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({ tags: [Tag.TYPESCRIPT] });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.tags).toContain(Tag.TYPESCRIPT);
      });

      it("複数のtagsで検索できる", async () => {
        const ops = getOperations();
        const index1 = createSearchIndex(60, { tags: [Tag.TYPESCRIPT] });
        const index2 = createSearchIndex(61, { tags: [Tag.REACT] });
        const index3 = createSearchIndex(62, { tags: [Tag.RUST] });

        await persistSearchIndex(firestore, ops, index1);
        await persistSearchIndex(firestore, ops, index2);
        await persistSearchIndex(firestore, ops, index3);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({ tags: [Tag.TYPESCRIPT, Tag.REACT] });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("sortByとorderで並び替えできる", async () => {
        const ops = getOperations();
        const now = new Date();
        const index1 = createSearchIndex(70, { title: "古い記事" });
        const index2 = createSearchIndex(71, { title: "新しい記事" });

        await persistSearchIndex(firestore, ops, index1);
        await persistSearchIndex(firestore, ops, index2);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({
          sortBy: Sort.NEWEST,
          order: Order.DESC,
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("LATESTソートを使用できる", async () => {
        const ops = getOperations();
        const index1 = createSearchIndex(80);
        const index2 = createSearchIndex(81);

        await persistSearchIndex(firestore, ops, index1);
        await persistSearchIndex(firestore, ops, index2);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({
          sortBy: Sort.LATEST,
          order: Order.DESC,
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("OLDESTソートを使用できる", async () => {
        const ops = getOperations();
        const index1 = createSearchIndex(90);
        const index2 = createSearchIndex(91);

        await persistSearchIndex(firestore, ops, index1);
        await persistSearchIndex(firestore, ops, index2);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({
          sortBy: Sort.OLDEST,
          order: Order.ASC,
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("複合条件で検索できる", async () => {
        const ops = getOperations();
        const index1 = createSearchIndex(100, {
          title: "TypeScript記事",
          type: ContentType.ARTICLE,
          tags: [Tag.TYPESCRIPT],
        });
        const index2 = createSearchIndex(101, {
          title: "TypeScriptメモ",
          type: ContentType.MEMO,
          tags: [Tag.TYPESCRIPT],
        });
        const index3 = createSearchIndex(102, {
          title: "Rust記事",
          type: ContentType.ARTICLE,
          tags: [Tag.RUST],
        });

        await persistSearchIndex(firestore, ops, index1);
        await persistSearchIndex(firestore, ops, index2);
        await persistSearchIndex(firestore, ops, index3);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({
          type: ContentType.ARTICLE,
          tags: [Tag.TYPESCRIPT],
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.title).toBe("TypeScript記事");
      });

      it("空の結果を返すことができる", async () => {
        const ops = getOperations();
        const index = createSearchIndex(110, { title: "React記事" });

        await persistSearchIndex(firestore, ops, index);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        const criteria = createCriteria({ freeWord: "存在しないキーワード" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(0);
      });

      it("freeWordが空文字の場合はngramフィルタを適用しない", async () => {
        const ops = getOperations();
        const index1 = createSearchIndex(120);
        const index2 = createSearchIndex(121);

        await persistSearchIndex(firestore, ops, index1);
        await persistSearchIndex(firestore, ops, index2);

        const repository = FirebaseSearchIndexRepository(firestore, ops);
        // freeWordがnullの場合は全件取得
        const criteria = createCriteria({ freeWord: null });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });
    });
  });
});
