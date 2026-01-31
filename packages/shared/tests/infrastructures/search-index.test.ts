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
  type SearchIndex,
  type Criteria,
} from "@shared/domains/search-index/common";
import type { TagIdentifier } from "@shared/domains/attributes/tag";
import { TagIdentifierMold } from "../support/molds/domains/attributes/tag";
import { Builder } from "../support/molds";
import {
  SearchIndexMold,
  CriteriaMold,
} from "../support/molds/domains/search-index/common";

describe("infrastructures/search-index", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnvironment = await createTestFirestoreWithSeed({});
    firestore = testEnvironment.firestore;
    operations = testEnvironment.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  const createTagIdentifier = (seed: number): TagIdentifier =>
    Builder(TagIdentifierMold).buildWith(seed);

  const TestTags = {
    TYPESCRIPT: createTagIdentifier(1001),
    REACT: createTagIdentifier(1002),
    RUST: createTagIdentifier(1003),
  };

  const createSearchIndex = (
    seed: number,
    overrides?: Partial<{
      title: string;
      excerpt: string;
      tags: TagIdentifier[];
      type:
        | typeof ContentType.ARTICLE
        | typeof ContentType.MEMO
        | typeof ContentType.SERIES;
    }>
  ): SearchIndex => {
    return Builder(SearchIndexMold).buildWith(seed, {
      title: overrides?.title,
      excerpt: overrides?.excerpt,
      tags: overrides?.tags ?? [TestTags.TYPESCRIPT],
      type: overrides?.type ?? ContentType.ARTICLE,
    });
  };

  const createCriteria = (params: {
    freeWord?: string | null;
    tags?: TagIdentifier[] | null;
    type?:
      | typeof ContentType.ALL
      | typeof ContentType.ARTICLE
      | typeof ContentType.MEMO
      | typeof ContentType.SERIES
      | null;
    sortBy?: typeof Sort.LATEST | typeof Sort.NEWEST | typeof Sort.OLDEST | null;
    order?: typeof Order.ASC | typeof Order.DESC | null;
  }): Criteria => {
    return Builder(CriteriaMold).buildWith(0, {
      freeWord: params.freeWord ?? null,
      tags: params.tags ?? null,
      type: params.type ?? null,
      sortBy: params.sortBy ?? null,
      order: params.order ?? null,
    });
  };

  const generateNgrams = (text: string, min = 2, max = 4): string[] => {
    const normalized = text
      .toLowerCase()
      .replace(/\s+/g, "")
      .replace(/[^\p{L}\p{N}]/gu, "");
    const ngrams: Set<string> = new Set();

    for (let ngramLength = min; ngramLength <= max; ngramLength++) {
      for (let index = 0; index <= normalized.length - ngramLength; index++) {
        ngrams.add(normalized.slice(index, index + ngramLength));
      }
    }

    return Array.from(ngrams);
  };

  const persistSearchIndex = async (
    firestoreInstance: Firestore,
    firestoreOperations: FirestoreOperations,
    index: SearchIndex
  ) => {
    const collection = firestoreOperations.collection(
      firestoreInstance,
      "search-index"
    );
    const document = firestoreOperations.doc(collection, index.identifier);

    await firestoreOperations.setDoc(document, {
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
      ngrams: generateNgrams(
        [index.title, index.excerpt, ...index.tags].join(" ")
      ),
      version: 1,
    });
  };

  describe("FirebaseSearchIndexRepository", () => {
    describe("search", () => {
      it("全ての検索インデックスを取得できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = createSearchIndex(1);
        const index2 = createSearchIndex(2);

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({});
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("freeWordでタイトルを検索できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = createSearchIndex(10, {
          title: "ZZZAlphaZZZ unique keyword",
          tags: [TestTags.RUST],
          excerpt: "記事Aの抜粋",
        });
        const index2 = createSearchIndex(11, {
          title: "YYYBetaYYY different content",
          tags: [TestTags.REACT],
          excerpt: "記事Bの抜粋",
        });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({ freeWord: "Alpha" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.title).toContain("Alpha");
      });

      it("freeWordで抜粋を検索できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = createSearchIndex(20, {
          title: "記事AAA",
          excerpt: "ZZZGammaZZZ unique content here",
          tags: [TestTags.RUST],
        });
        const index2 = createSearchIndex(21, {
          title: "記事BBB",
          excerpt: "YYYDeltaYYY different content here",
          tags: [TestTags.REACT],
        });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({ freeWord: "Gamma" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.excerpt).toContain("Gamma");
      });

      it("typeで記事を検索できる", async () => {
        const firestoreOperations = getOperations();
        const articleIndex = createSearchIndex(30, {
          type: ContentType.ARTICLE,
        });
        const memoIndex = createSearchIndex(31, { type: ContentType.MEMO });

        await persistSearchIndex(firestore, firestoreOperations, articleIndex);
        await persistSearchIndex(firestore, firestoreOperations, memoIndex);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({ type: ContentType.ARTICLE });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.type).toBe(ContentType.ARTICLE);
      });

      it("typeでメモを検索できる", async () => {
        const firestoreOperations = getOperations();
        const articleIndex = createSearchIndex(40, {
          type: ContentType.ARTICLE,
        });
        const memoIndex = createSearchIndex(41, { type: ContentType.MEMO });

        await persistSearchIndex(firestore, firestoreOperations, articleIndex);
        await persistSearchIndex(firestore, firestoreOperations, memoIndex);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({ type: ContentType.MEMO });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.type).toBe(ContentType.MEMO);
      });

      it("tagsで検索できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = createSearchIndex(50, {
          tags: [TestTags.TYPESCRIPT, TestTags.REACT],
        });
        const index2 = createSearchIndex(51, { tags: [TestTags.RUST] });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({ tags: [TestTags.TYPESCRIPT] });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.tags).toContain(TestTags.TYPESCRIPT);
      });

      it("複数のtagsで検索できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = createSearchIndex(60, { tags: [TestTags.TYPESCRIPT] });
        const index2 = createSearchIndex(61, { tags: [TestTags.REACT] });
        const index3 = createSearchIndex(62, { tags: [TestTags.RUST] });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);
        await persistSearchIndex(firestore, firestoreOperations, index3);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({
          tags: [TestTags.TYPESCRIPT, TestTags.REACT],
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("sortByとorderで並び替えできる", async () => {
        const firestoreOperations = getOperations();
        const index1 = createSearchIndex(70, { title: "古い記事" });
        const index2 = createSearchIndex(71, { title: "新しい記事" });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({
          sortBy: Sort.NEWEST,
          order: Order.DESC,
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("LATESTソートを使用できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = createSearchIndex(80);
        const index2 = createSearchIndex(81);

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({
          sortBy: Sort.LATEST,
          order: Order.DESC,
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("OLDESTソートを使用できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = createSearchIndex(90);
        const index2 = createSearchIndex(91);

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({
          sortBy: Sort.OLDEST,
          order: Order.ASC,
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("複合条件で検索できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = createSearchIndex(100, {
          title: "TypeScript記事",
          type: ContentType.ARTICLE,
          tags: [TestTags.TYPESCRIPT],
        });
        const index2 = createSearchIndex(101, {
          title: "TypeScriptメモ",
          type: ContentType.MEMO,
          tags: [TestTags.TYPESCRIPT],
        });
        const index3 = createSearchIndex(102, {
          title: "Rust記事",
          type: ContentType.ARTICLE,
          tags: [TestTags.RUST],
        });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);
        await persistSearchIndex(firestore, firestoreOperations, index3);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({
          type: ContentType.ARTICLE,
          tags: [TestTags.TYPESCRIPT],
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.title).toBe("TypeScript記事");
      });

      it("空の結果を返すことができる", async () => {
        const firestoreOperations = getOperations();
        const index = createSearchIndex(110, { title: "React記事" });

        await persistSearchIndex(firestore, firestoreOperations, index);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({ freeWord: "存在しないキーワード" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(0);
      });

      it("freeWordが空文字の場合はngramフィルタを適用しない", async () => {
        const firestoreOperations = getOperations();
        const index1 = createSearchIndex(120);
        const index2 = createSearchIndex(121);

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = createCriteria({ freeWord: null });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });
    });
  });
});
