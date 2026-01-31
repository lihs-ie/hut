import { describe, it, expect, beforeEach } from "vitest";
import { FirebaseMemoRepository } from "@shared/infrastructures/memos";
import {
  createTestFirestoreWithSeed,
  clearFirestore,
  type Firestore,
} from "../support/mock/firebase/firestore";
import { Builder } from "../support/molds";
import {
  MemoFactory,
  MemoIdentifierFactory,
  MemoSlugFactory,
  MemoEntryFactory,
} from "../support/molds/domains/memo/common";
import {
  isAggregateNotFoundError,
  isDuplicationError,
} from "@shared/aspects/error";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { validateCriteria, type Criteria } from "@shared/domains/memo";
import { PublishStatus } from "@shared/domains/common";
import type { TagIdentifier } from "@shared/domains/attributes/tag";
import { TagIdentifierMold } from "../support/molds/domains/attributes/tag";

describe("infrastructures/memos", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnv = await createTestFirestoreWithSeed({});
    firestore = testEnv.firestore;
    operations = testEnv.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  // テスト用のTagIdentifier定数
  const createTagIdentifier = (seed: number): TagIdentifier =>
    Builder(TagIdentifierMold).buildWith(seed);

  const TestTags = {
    TYPESCRIPT: createTagIdentifier(1001),
    REACT: createTagIdentifier(1002),
    RUST: createTagIdentifier(1003),
  };

  const createCriteria = (params: {
    tags?: TagIdentifier[] | null;
    freeWord?: string | null;
    status?: PublishStatus | null;
  }): Criteria => {
    return validateCriteria({
      tags: params.tags ?? null,
      freeWord: params.freeWord ?? null,
      status: params.status ?? null,
    }).unwrap();
  };

  describe("FirebaseMemoRepository", () => {
    describe("persist", () => {
      it("新しいメモを保存できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memo = Builder(MemoFactory).buildWith(1);

        const result = await repository.persist(memo).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(memo.identifier).unwrap();
        expect(found.identifier).toBe(memo.identifier);
        expect(found.title).toBe(memo.title);
      });

      it("既存のメモを更新できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memo = Builder(MemoFactory).buildWith(2);

        await repository.persist(memo).unwrap();

        const updatedMemo = Builder(MemoFactory).duplicate(memo, {
          title: "Updated Title" as typeof memo.title,
        });

        const result = await repository.persist(updatedMemo).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(memo.identifier).unwrap();
        expect(found.title).toBe("Updated Title");
      });

      it("存在しないメモを更新しようとするとエラーになる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memo = Builder(MemoFactory).buildWith(3);

        await repository.persist(memo).unwrap();

        clearFirestore(firestore);

        const updatedMemo = Builder(MemoFactory).duplicate(memo, {
          title: "Updated Title" as typeof memo.title,
        });

        const result = await repository.persist(updatedMemo).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });

      it("重複するメモを作成しようとするとエラーになる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memo = Builder(MemoFactory).buildWith(4);

        await repository.persist(memo).unwrap();

        const repository2 = FirebaseMemoRepository(firestore, getOperations());
        const duplicateMemo = Builder(MemoFactory).buildWith(5, {
          identifier: memo.identifier,
        });

        const result = await repository2.persist(duplicateMemo).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });
    });

    describe("find", () => {
      it("存在するメモを取得できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memo = Builder(MemoFactory).buildWith(10);

        await repository.persist(memo).unwrap();

        const found = await repository.find(memo.identifier).unwrap();

        expect(found.identifier).toBe(memo.identifier);
        expect(found.title).toBe(memo.title);
        expect(found.slug).toBe(memo.slug);
      });

      it("存在しないメモを取得しようとするとエラーになる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const identifier = Builder(MemoIdentifierFactory).buildWith(11);

        const result = await repository.find(identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("findBySlug", () => {
      it("slugでメモを取得できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const slug = Builder(MemoSlugFactory).buildWith(20, {
          value: "test-memo-slug",
        });
        const memo = Builder(MemoFactory).buildWith(21, { slug });

        await repository.persist(memo).unwrap();

        const found = await repository.findBySlug(slug).unwrap();

        expect(found.identifier).toBe(memo.identifier);
        expect(found.slug).toBe(slug);
      });

      it("存在しないslugで検索するとエラーになる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const slug = Builder(MemoSlugFactory).buildWith(22, {
          value: "non-existent-slug",
        });

        const result = await repository.findBySlug(slug).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("ofIdentifiers", () => {
      it("複数のメモを取得できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memos = Builder(MemoFactory).buildListWith(3, 30).toArray();

        for (const memo of memos) {
          await repository.persist(memo).unwrap();
        }

        const identifiers = memos.map((m) => m.identifier);
        const found = await repository.ofIdentifiers(identifiers).unwrap();

        expect(found.length).toBe(3);
      });

      it("空の配列を渡すと空の配列を返す", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());

        const found = await repository.ofIdentifiers([]).unwrap();

        expect(found.length).toBe(0);
      });

      it("存在しないメモが含まれるとエラーになる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memo = Builder(MemoFactory).buildWith(35);

        await repository.persist(memo).unwrap();

        const nonExistentIdentifier = Builder(MemoIdentifierFactory).buildWith(36);
        const identifiers = [memo.identifier, nonExistentIdentifier];

        const result = await repository.ofIdentifiers(identifiers, true).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("search", () => {
      it("全てのメモを取得できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memos = Builder(MemoFactory).buildListWith(5, 40).toArray();

        for (const memo of memos) {
          await repository.persist(memo).unwrap();
        }

        const criteria = createCriteria({});
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(5);
      });

      it("tagでメモを検索できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memo1 = Builder(MemoFactory).buildWith(50, {
          tags: [TestTags.TYPESCRIPT, TestTags.REACT],
        });
        const memo2 = Builder(MemoFactory).buildWith(51, {
          tags: [TestTags.RUST],
        });

        await repository.persist(memo1).unwrap();
        await repository.persist(memo2).unwrap();

        const criteria = createCriteria({ tags: [TestTags.TYPESCRIPT] });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.identifier).toBe(memo1.identifier);
      });

      it("freeWordでタイトルを検索できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memo1 = Builder(MemoFactory).buildWith(60, {
          title: "TypeScriptメモ" as typeof memo1.title,
        });
        const memo2 = Builder(MemoFactory).buildWith(61, {
          title: "Pythonメモ" as typeof memo2.title,
        });

        await repository.persist(memo1).unwrap();
        await repository.persist(memo2).unwrap();

        const criteria = createCriteria({ freeWord: "TypeScript" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.title).toBe("TypeScriptメモ");
      });

      it("freeWordでエントリのテキストを検索できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const entries1 = [
          Builder(MemoEntryFactory).buildWith(70, {
            text: "This is about GraphQL",
          }),
        ];
        const entries2 = [
          Builder(MemoEntryFactory).buildWith(71, {
            text: "This is about REST API",
          }),
        ];
        const memo1 = Builder(MemoFactory).buildWith(72, { entries: entries1 });
        const memo2 = Builder(MemoFactory).buildWith(73, { entries: entries2 });

        await repository.persist(memo1).unwrap();
        await repository.persist(memo2).unwrap();

        const criteria = createCriteria({ freeWord: "GraphQL" });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.identifier).toBe(memo1.identifier);
      });
    });

    describe("slug uniqueness", () => {
      it("同じslugで新しいメモを作成しようとするとエラーになる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const slug = Builder(MemoSlugFactory).buildWith(200, {
          value: "duplicate-slug",
        });
        const memo1 = Builder(MemoFactory).buildWith(201, { slug });
        const memo2 = Builder(MemoFactory).buildWith(202, { slug });

        await repository.persist(memo1).unwrap();

        const result = await repository.persist(memo2).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });

      it("メモ更新時に他のメモと同じslugに変更しようとするとエラーになる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const slug1 = Builder(MemoSlugFactory).buildWith(210, {
          value: "slug-one",
        });
        const slug2 = Builder(MemoSlugFactory).buildWith(211, {
          value: "slug-two",
        });

        const memo1 = Builder(MemoFactory).buildWith(212, { slug: slug1 });
        const memo2 = Builder(MemoFactory).buildWith(213, { slug: slug2 });

        await repository.persist(memo1).unwrap();
        await repository.persist(memo2).unwrap();

        const updatedMemo2 = Builder(MemoFactory).duplicate(memo2, {
          slug: slug1,
        });

        const result = await repository.persist(updatedMemo2).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });

      it("メモ削除後に同じslugで新しいメモを作成できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const slug = Builder(MemoSlugFactory).buildWith(220, {
          value: "reusable-slug",
        });
        const memo1 = Builder(MemoFactory).buildWith(221, { slug });

        await repository.persist(memo1).unwrap();
        await repository.terminate(memo1.identifier).unwrap();

        const memo2 = Builder(MemoFactory).buildWith(222, { slug });
        const result = await repository.persist(memo2).unwrap();

        expect(result).toBeUndefined();
      });

      it("同じメモのslugを変更できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const oldSlug = Builder(MemoSlugFactory).buildWith(230, {
          value: "old-slug",
        });
        const newSlug = Builder(MemoSlugFactory).buildWith(231, {
          value: "new-slug",
        });

        const memo = Builder(MemoFactory).buildWith(232, { slug: oldSlug });

        await repository.persist(memo).unwrap();

        const updatedMemo = Builder(MemoFactory).duplicate(memo, {
          slug: newSlug,
        });

        const result = await repository.persist(updatedMemo).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(memo.identifier).unwrap();
        expect(found.slug).toBe(newSlug);
      });
    });

    describe("terminate", () => {
      it("メモを削除できる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const memo = Builder(MemoFactory).buildWith(90);

        await repository.persist(memo).unwrap();

        const result = await repository.terminate(memo.identifier).unwrap();

        expect(result).toBeUndefined();

        const findResult = await repository.find(memo.identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(findResult).not.toBeNull();
        expect(isAggregateNotFoundError(findResult)).toBe(true);
      });

      it("存在しないメモを削除しようとするとエラーになる", async () => {
        const repository = FirebaseMemoRepository(firestore, getOperations());
        const identifier = Builder(MemoIdentifierFactory).buildWith(91);

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
