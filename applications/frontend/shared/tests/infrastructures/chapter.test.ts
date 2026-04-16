import { describe, it, expect, beforeEach } from "vitest";
import { Timestamp } from "firebase/firestore";
import { FirebaseChapterRepository } from "@shared/infrastructures/chapter";
import {
  createTestFirestoreWithSeed,
  clearFirestore,
  seedFirestore,
  type Firestore,
} from "../support/mock/firebase/firestore";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ChapterMold,
  ChapterIdentifierMold,
} from "../support/molds/domains/series/chapter";
import { SlugMold } from "../support/molds/domains/common/slug";
import {
  isAggregateNotFoundError,
  isDuplicationError,
} from "@shared/aspects/error";
import type { FirestoreOperations } from "@shared/infrastructures/common";

describe("infrastructures/chapter", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnv = await createTestFirestoreWithSeed({});
    firestore = testEnv.firestore;
    operations = testEnv.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  describe("FirebaseChapterRepository", () => {
    describe("find", () => {
      it("存在するChapterを取得できる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(10);

        await repository.persist(chapter).unwrap();

        const found = await repository.find(chapter.identifier).unwrap();

        expect(found.identifier).toBe(chapter.identifier);
        expect(found.title).toBe(chapter.title);
        expect(found.slug).toBe(chapter.slug);
        expect(found.content).toBe(chapter.content);
      });

      it("存在しないChapterを取得しようとするとエラーになる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const identifier = Forger(ChapterIdentifierMold).forgeWithSeed(11);

        const result = await repository.find(identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("findBySlug", () => {
      it("slugでChapterを取得できる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const targetSlug = Forger(SlugMold).forgeWithSeed(12, {
          value: "find-by-slug-chapter-test",
        });
        const chapter = Forger(ChapterMold).forgeWithSeed(13, {
          slug: targetSlug,
        });

        await repository.persist(chapter).unwrap();

        const found = await repository.findBySlug(targetSlug).unwrap();

        expect(found.identifier).toBe(chapter.identifier);
        expect(found.slug).toBe(targetSlug);
      });

      it("存在しないslugで検索するとエラーになる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const nonExistentSlug = Forger(SlugMold).forgeWithSeed(14, {
          value: "non-existent-chapter-slug",
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
      it("複数のChapterを取得できる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapterList = Forger(ChapterMold).forgeMultiWithSeed(3, 20);

        for (const chapter of chapterList) {
          await repository.persist(chapter).unwrap();
        }

        const identifiers = chapterList.map((chapter) => chapter.identifier);
        const found = await repository.ofIdentifiers(identifiers).unwrap();

        expect(found.length).toBe(3);
      });

      it("空の配列を渡すと空の配列を返す", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );

        const found = await repository.ofIdentifiers([]).unwrap();

        expect(found.length).toBe(0);
      });

      it("存在しないChapterが含まれ throwOnMissing が true の場合エラーになる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(25);

        await repository.persist(chapter).unwrap();

        const nonExistentIdentifier =
          Forger(ChapterIdentifierMold).forgeWithSeed(26);
        const identifiers = [chapter.identifier, nonExistentIdentifier];

        const result = await repository
          .ofIdentifiers(identifiers, true)
          .match({
            ok: () => null,
            err: (error) => error,
          });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });

      it("存在しないChapterが含まれ throwOnMissing が false の場合はスキップされる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(27);

        await repository.persist(chapter).unwrap();

        const nonExistentIdentifier =
          Forger(ChapterIdentifierMold).forgeWithSeed(28);
        const identifiers = [chapter.identifier, nonExistentIdentifier];

        const found = await repository
          .ofIdentifiers(identifiers, false)
          .unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.identifier).toBe(chapter.identifier);
      });
    });

    describe("persist", () => {
      it("新しいChapterを保存できる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(1);

        const result = await repository.persist(chapter).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(chapter.identifier).unwrap();
        expect(found.identifier).toBe(chapter.identifier);
        expect(found.title).toBe(chapter.title);
      });

      it("既存のChapterを更新できる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(2);

        await repository.persist(chapter).unwrap();

        const updatedChapter = Forger(ChapterMold).forgeWithSeed(0, {
          ...chapter,
          title: "Updated Chapter Title" as typeof chapter.title,
        });

        const result = await repository.persist(updatedChapter).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(chapter.identifier).unwrap();
        expect(found.title).toBe("Updated Chapter Title");
      });

      it("存在しないChapterを更新しようとするとエラーになる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(3);

        await repository.persist(chapter).unwrap();

        clearFirestore(firestore);

        const updatedChapter = Forger(ChapterMold).forgeWithSeed(0, {
          ...chapter,
          title: "Updated Title" as typeof chapter.title,
        });

        const result = await repository.persist(updatedChapter).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });

      it("重複するChapterを作成しようとするとエラーになる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(4);

        await repository.persist(chapter).unwrap();

        const repository2 = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const duplicateChapter = Forger(ChapterMold).forgeWithSeed(5, {
          identifier: chapter.identifier,
        });

        const result = await repository2.persist(duplicateChapter).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });

      it("imagesフィールドが正しく保存・取得できる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(6);

        await repository.persist(chapter).unwrap();

        const found = await repository.find(chapter.identifier).unwrap();
        expect(found.images).toEqual(chapter.images);
      });

      it("statusフィールドが正しく保存・取得できる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(7);

        await repository.persist(chapter).unwrap();

        const found = await repository.find(chapter.identifier).unwrap();
        expect(found.status).toBe(chapter.status);
      });
    });

    describe("terminate", () => {
      it("Chapterを削除できる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(60);

        await repository.persist(chapter).unwrap();

        const result = await repository.terminate(chapter.identifier).unwrap();

        expect(result).toBeUndefined();

        const findResult = await repository.find(chapter.identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(findResult).not.toBeNull();
        expect(isAggregateNotFoundError(findResult)).toBe(true);
      });

      it("存在しないChapterを削除しようとするとエラーになる", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const identifier = Forger(ChapterIdentifierMold).forgeWithSeed(61);

        const result = await repository.terminate(identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("publishedAt の永続化", () => {
      it("publishedAt が Date として保存・復元される", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const publishedAt = new Date("2025-01-01T00:00:00Z");
        const chapter = Forger(ChapterMold).forgeWithSeed(950, {
          publishedAt,
        });

        await repository.persist(chapter).unwrap();

        const found = await repository.find(chapter.identifier).unwrap();

        expect(found.publishedAt).toBeInstanceOf(Date);
        expect(found.publishedAt?.getTime()).toBe(publishedAt.getTime());
      });

      it("publishedAt が null として保存・復元される", async () => {
        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const chapter = Forger(ChapterMold).forgeWithSeed(951, {
          publishedAt: null,
        });

        await repository.persist(chapter).unwrap();

        const found = await repository.find(chapter.identifier).unwrap();

        expect(found.publishedAt).toBeNull();
      });

      it("Firestore に publishedAt フィールドが存在しない場合は null として復元される", async () => {
        const identifier = Forger(ChapterIdentifierMold).forgeWithSeed(952);
        const createdAt = Timestamp.fromDate(new Date("2024-01-01T00:00:00Z"));
        const updatedAt = Timestamp.fromDate(new Date("2024-01-02T00:00:00Z"));

        await seedFirestore(firestore, {
          chapters: {
            [identifier]: {
              identifier,
              title: "Legacy Chapter",
              slug: "legacy-chapter",
              content: "Legacy content",
              images: [],
              status: "published",
              timeline: {
                createdAt,
                updatedAt,
              },
              version: 1,
            },
          },
        });

        const repository = FirebaseChapterRepository(
          firestore,
          getOperations(),
        );
        const found = await repository.find(identifier).unwrap();

        expect(found.publishedAt).toBeNull();
      });
    });
  });
});
