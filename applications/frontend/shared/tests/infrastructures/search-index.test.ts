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
} from "@shared/domains/search-index/common";
import { TagIdentifierMold } from "../support/molds/domains/attributes/tag";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SearchIndexMold,
  SearchIndexTitleMold,
  SearchIndexExcerptMold,
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

  const TestTags = {
    TYPESCRIPT: Forger(TagIdentifierMold).forgeWithSeed(1001),
    REACT: Forger(TagIdentifierMold).forgeWithSeed(1002),
    RUST: Forger(TagIdentifierMold).forgeWithSeed(1003),
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
        const index1 = Forger(SearchIndexMold).forgeWithSeed(1);
        const index2 = Forger(SearchIndexMold).forgeWithSeed(2);

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0);
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("freeWordでタイトルを検索できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = Forger(SearchIndexMold).forgeWithSeed(10, {
          title: Forger(SearchIndexTitleMold).forgeWithSeed(0, { value: "ZZZAlphaZZZ unique keyword" }),
          tags: [TestTags.RUST],
          excerpt: Forger(SearchIndexExcerptMold).forgeWithSeed(0, { value: "記事Aの抜粋" }),
        });
        const index2 = Forger(SearchIndexMold).forgeWithSeed(11, {
          title: Forger(SearchIndexTitleMold).forgeWithSeed(0, { value: "YYYBetaYYY different content" }),
          tags: [TestTags.REACT],
          excerpt: Forger(SearchIndexExcerptMold).forgeWithSeed(0, { value: "記事Bの抜粋" }),
        });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, { freeWord: "Alpha" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.title).toContain("Alpha");
      });

      it("freeWordで抜粋を検索できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = Forger(SearchIndexMold).forgeWithSeed(20, {
          title: Forger(SearchIndexTitleMold).forgeWithSeed(0, { value: "記事AAA" }),
          excerpt: Forger(SearchIndexExcerptMold).forgeWithSeed(0, { value: "ZZZGammaZZZ unique content here" }),
          tags: [TestTags.RUST],
        });
        const index2 = Forger(SearchIndexMold).forgeWithSeed(21, {
          title: Forger(SearchIndexTitleMold).forgeWithSeed(0, { value: "記事BBB" }),
          excerpt: Forger(SearchIndexExcerptMold).forgeWithSeed(0, { value: "YYYDeltaYYY different content here" }),
          tags: [TestTags.REACT],
        });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, { freeWord: "Gamma" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.excerpt).toContain("Gamma");
      });

      it("typeで記事を検索できる", async () => {
        const firestoreOperations = getOperations();
        const articleIndex = Forger(SearchIndexMold).forgeWithSeed(30, {
          type: ContentType.ARTICLE,
        });
        const memoIndex = Forger(SearchIndexMold).forgeWithSeed(31, {
          type: ContentType.MEMO,
        });

        await persistSearchIndex(firestore, firestoreOperations, articleIndex);
        await persistSearchIndex(firestore, firestoreOperations, memoIndex);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, { type: ContentType.ARTICLE });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.type).toBe(ContentType.ARTICLE);
      });

      it("typeでメモを検索できる", async () => {
        const firestoreOperations = getOperations();
        const articleIndex = Forger(SearchIndexMold).forgeWithSeed(40, {
          type: ContentType.ARTICLE,
        });
        const memoIndex = Forger(SearchIndexMold).forgeWithSeed(41, {
          type: ContentType.MEMO,
        });

        await persistSearchIndex(firestore, firestoreOperations, articleIndex);
        await persistSearchIndex(firestore, firestoreOperations, memoIndex);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, { type: ContentType.MEMO });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.type).toBe(ContentType.MEMO);
      });

      it("tagsで検索できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = Forger(SearchIndexMold).forgeWithSeed(50, {
          tags: [TestTags.TYPESCRIPT, TestTags.REACT],
        });
        const index2 = Forger(SearchIndexMold).forgeWithSeed(51, {
          tags: [TestTags.RUST],
        });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, { tags: [TestTags.TYPESCRIPT] });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.tags).toContain(TestTags.TYPESCRIPT);
      });

      it("複数のtagsで検索できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = Forger(SearchIndexMold).forgeWithSeed(60, {
          tags: [TestTags.TYPESCRIPT],
        });
        const index2 = Forger(SearchIndexMold).forgeWithSeed(61, {
          tags: [TestTags.REACT],
        });
        const index3 = Forger(SearchIndexMold).forgeWithSeed(62, {
          tags: [TestTags.RUST],
        });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);
        await persistSearchIndex(firestore, firestoreOperations, index3);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, {
          tags: [TestTags.TYPESCRIPT, TestTags.REACT],
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("sortByとorderで並び替えできる", async () => {
        const firestoreOperations = getOperations();
        const index1 = Forger(SearchIndexMold).forgeWithSeed(70, {
          title: Forger(SearchIndexTitleMold).forgeWithSeed(0, { value: "古い記事" }),
        });
        const index2 = Forger(SearchIndexMold).forgeWithSeed(71, {
          title: Forger(SearchIndexTitleMold).forgeWithSeed(0, { value: "新しい記事" }),
        });

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, {
          sortBy: Sort.NEWEST,
          order: Order.DESC,
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("LATESTソートを使用できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = Forger(SearchIndexMold).forgeWithSeed(80);
        const index2 = Forger(SearchIndexMold).forgeWithSeed(81);

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, {
          sortBy: Sort.LATEST,
          order: Order.DESC,
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("OLDESTソートを使用できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = Forger(SearchIndexMold).forgeWithSeed(90);
        const index2 = Forger(SearchIndexMold).forgeWithSeed(91);

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, {
          sortBy: Sort.OLDEST,
          order: Order.ASC,
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });

      it("複合条件で検索できる", async () => {
        const firestoreOperations = getOperations();
        const index1 = Forger(SearchIndexMold).forgeWithSeed(100, {
          title: Forger(SearchIndexTitleMold).forgeWithSeed(0, { value: "TypeScript記事" }),
          type: ContentType.ARTICLE,
          tags: [TestTags.TYPESCRIPT],
        });
        const index2 = Forger(SearchIndexMold).forgeWithSeed(101, {
          title: Forger(SearchIndexTitleMold).forgeWithSeed(0, { value: "TypeScriptメモ" }),
          type: ContentType.MEMO,
          tags: [TestTags.TYPESCRIPT],
        });
        const index3 = Forger(SearchIndexMold).forgeWithSeed(102, {
          title: Forger(SearchIndexTitleMold).forgeWithSeed(0, { value: "Rust記事" }),
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
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, {
          type: ContentType.ARTICLE,
          tags: [TestTags.TYPESCRIPT],
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.title).toBe("TypeScript記事");
      });

      it("空の結果を返すことができる", async () => {
        const firestoreOperations = getOperations();
        const index = Forger(SearchIndexMold).forgeWithSeed(110, {
          title: Forger(SearchIndexTitleMold).forgeWithSeed(0, { value: "React記事" }),
        });

        await persistSearchIndex(firestore, firestoreOperations, index);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, { freeWord: "存在しないキーワード" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(0);
      });

      it("freeWordが空文字の場合はngramフィルタを適用しない", async () => {
        const firestoreOperations = getOperations();
        const index1 = Forger(SearchIndexMold).forgeWithSeed(120);
        const index2 = Forger(SearchIndexMold).forgeWithSeed(121);

        await persistSearchIndex(firestore, firestoreOperations, index1);
        await persistSearchIndex(firestore, firestoreOperations, index2);

        const repository = FirebaseSearchIndexRepository(
          firestore,
          firestoreOperations
        );
        const criteria = Forger(CriteriaMold).forgeWithSeed(0, { freeWord: null });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(2);
      });
    });
  });
});
