import { describe, it, expect, beforeEach } from "vitest";
import { FirebaseSeriesRepository } from "@shared/infrastructures/series";
import {
  createTestFirestoreWithSeed,
  clearFirestore,
  type Firestore,
} from "../support/mock/firebase/firestore";
import { Builder } from "../support/molds";
import {
  SeriesFactory,
  SeriesIdentifierFactory,
  SeriesSlugFactory,
  SeriesSlugMold,
} from "../support/molds/domains/series/common";
import {
  isAggregateNotFoundError,
  isDuplicationError,
} from "@shared/aspects/error";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { validateCriteria, type Criteria, type SeriesSlug } from "@shared/domains/series";
import type { TagIdentifier } from "@shared/domains/attributes/tag";

describe("infrastructures/series", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnv = await createTestFirestoreWithSeed({});
    firestore = testEnv.firestore;
    operations = testEnv.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  const createCriteria = (params: {
    slug?: SeriesSlug | null;
    tags?: TagIdentifier[] | null;
  }): Criteria => {
    return validateCriteria({
      slug: params.slug ?? null,
      tags: params.tags ?? null,
    }).unwrap();
  };

  describe("FirebaseSeriesRepository", () => {
    describe("persist", () => {
      it("新しいシリーズを保存できる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const series = Builder(SeriesFactory).buildWith(1);

        const result = await repository.persist(series).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(series.identifier).unwrap();
        expect(found.identifier).toBe(series.identifier);
        expect(found.title).toBe(series.title);
      });

      it("既存のシリーズを更新できる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const series = Builder(SeriesFactory).buildWith(2);

        await repository.persist(series).unwrap();

        const updatedSeries = Builder(SeriesFactory).duplicate(series, {
          title: "Updated Series Title" as typeof series.title,
        });

        const result = await repository.persist(updatedSeries).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(series.identifier).unwrap();
        expect(found.title).toBe("Updated Series Title");
      });

      it("存在しないシリーズを更新しようとするとエラーになる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const series = Builder(SeriesFactory).buildWith(3);

        await repository.persist(series).unwrap();

        clearFirestore(firestore);

        const updatedSeries = Builder(SeriesFactory).duplicate(series, {
          title: "Updated Title" as typeof series.title,
        });

        const result = await repository.persist(updatedSeries).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });

      it("重複するシリーズを作成しようとするとエラーになる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const series = Builder(SeriesFactory).buildWith(4);

        await repository.persist(series).unwrap();

        const repository2 = FirebaseSeriesRepository(
          firestore,
          getOperations(),
        );
        const duplicateSeries = Builder(SeriesFactory).buildWith(5, {
          identifier: series.identifier,
        });

        const result = await repository2.persist(duplicateSeries).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });
    });

    describe("find", () => {
      it("存在するシリーズを取得できる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const series = Builder(SeriesFactory).buildWith(10);

        await repository.persist(series).unwrap();

        const found = await repository.find(series.identifier).unwrap();

        expect(found.identifier).toBe(series.identifier);
        expect(found.title).toBe(series.title);
        expect(found.slug).toBe(series.slug);
        expect(found.description).toBe(series.description);
      });

      it("存在しないシリーズを取得しようとするとエラーになる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const identifier = Builder(SeriesIdentifierFactory).buildWith(11);

        const result = await repository.find(identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("findBySlug", () => {
      it("slugでシリーズを取得できる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const targetSlug = Builder(SeriesSlugFactory).buildWith(12, {
          value: "find-by-slug-test",
        });
        const series = Builder(SeriesFactory).buildWith(13, {
          slug: targetSlug,
        });

        await repository.persist(series).unwrap();

        const found = await repository.findBySlug(targetSlug).unwrap();

        expect(found.identifier).toBe(series.identifier);
        expect(found.slug).toBe(targetSlug);
      });

      it("存在しないslugで検索するとエラーになる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const nonExistentSlug = Builder(SeriesSlugFactory).buildWith(14, {
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
      it("複数のシリーズを取得できる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const seriesList = Builder(SeriesFactory)
          .buildListWith(3, 20)
          .toArray();

        for (const series of seriesList) {
          await repository.persist(series).unwrap();
        }

        const identifiers = seriesList.map((s) => s.identifier);
        const found = await repository.ofIdentifiers(identifiers).unwrap();

        expect(found.length).toBe(3);
      });

      it("空の配列を渡すと空の配列を返す", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());

        const found = await repository.ofIdentifiers([]).unwrap();

        expect(found.length).toBe(0);
      });

      it("存在しないシリーズが含まれるとエラーになる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const series = Builder(SeriesFactory).buildWith(25);

        await repository.persist(series).unwrap();

        const nonExistentIdentifier = Builder(SeriesIdentifierFactory).buildWith(26);
        const identifiers = [series.identifier, nonExistentIdentifier];

        const result = await repository.ofIdentifiers(identifiers, true).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("search", () => {
      it("全てのシリーズを取得できる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const seriesList = Builder(SeriesFactory)
          .buildListWith(5, 30)
          .toArray();

        for (const series of seriesList) {
          await repository.persist(series).unwrap();
        }

        const criteria = createCriteria({});
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(5);
      });

      it("タイトルでシリーズを検索できる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const series1 = Builder(SeriesFactory).buildWith(40, {
          title: "typescript-series" as typeof series1.title,
        });
        const series2 = Builder(SeriesFactory).buildWith(41, {
          title: "rust-series" as typeof series2.title,
        });

        await repository.persist(series1).unwrap();
        await repository.persist(series2).unwrap();

        const searchSlug = Builder(SeriesSlugMold).buildWith(1000, { value: "typescript" });
        const criteria = createCriteria({ slug: searchSlug });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.title).toBe("typescript-series");
      });

      it("大文字小文字を区別しない検索ができる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const series = Builder(SeriesFactory).buildWith(50, {
          title: "javascript-basics" as typeof series.title,
        });

        await repository.persist(series).unwrap();

        const searchSlug = Builder(SeriesSlugMold).buildWith(1001, { value: "javascript" });
        const criteria = createCriteria({ slug: searchSlug });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.title).toBe("javascript-basics");
      });

      it("マッチしない検索語では空の配列を返す", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const series = Builder(SeriesFactory).buildWith(55, {
          title: "react-tutorial" as typeof series.title,
        });

        await repository.persist(series).unwrap();

        const searchSlug = Builder(SeriesSlugMold).buildWith(1002, { value: "angular" });
        const criteria = createCriteria({ slug: searchSlug });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(0);
      });
    });

    describe("slug uniqueness", () => {
      it("同じslugで新しいシリーズを作成しようとするとエラーになる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const slug = Builder(SeriesSlugFactory).buildWith(200, {
          value: "duplicate-slug",
        });
        const series1 = Builder(SeriesFactory).buildWith(201, { slug });
        const series2 = Builder(SeriesFactory).buildWith(202, { slug });

        await repository.persist(series1).unwrap();

        const result = await repository.persist(series2).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });

      it("シリーズ更新時に他のシリーズと同じslugに変更しようとするとエラーになる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const slug1 = Builder(SeriesSlugFactory).buildWith(210, {
          value: "slug-one",
        });
        const slug2 = Builder(SeriesSlugFactory).buildWith(211, {
          value: "slug-two",
        });

        const series1 = Builder(SeriesFactory).buildWith(212, { slug: slug1 });
        const series2 = Builder(SeriesFactory).buildWith(213, { slug: slug2 });

        await repository.persist(series1).unwrap();
        await repository.persist(series2).unwrap();

        const updatedSeries2 = Builder(SeriesFactory).duplicate(series2, {
          slug: slug1,
        });

        const result = await repository.persist(updatedSeries2).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });

      it("シリーズ削除後に同じslugで新しいシリーズを作成できる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const slug = Builder(SeriesSlugFactory).buildWith(220, {
          value: "reusable-slug",
        });
        const series1 = Builder(SeriesFactory).buildWith(221, { slug });

        await repository.persist(series1).unwrap();
        await repository.terminate(series1.identifier).unwrap();

        const series2 = Builder(SeriesFactory).buildWith(222, { slug });
        const result = await repository.persist(series2).unwrap();

        expect(result).toBeUndefined();
      });

      it("同じシリーズのslugを変更できる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const oldSlug = Builder(SeriesSlugFactory).buildWith(230, {
          value: "old-slug",
        });
        const newSlug = Builder(SeriesSlugFactory).buildWith(231, {
          value: "new-slug",
        });

        const series = Builder(SeriesFactory).buildWith(232, { slug: oldSlug });

        await repository.persist(series).unwrap();

        const updatedSeries = Builder(SeriesFactory).duplicate(series, {
          slug: newSlug,
        });

        const result = await repository.persist(updatedSeries).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(series.identifier).unwrap();
        expect(found.slug).toBe(newSlug);
      });
    });

    describe("terminate", () => {
      it("シリーズを削除できる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const series = Builder(SeriesFactory).buildWith(60);

        await repository.persist(series).unwrap();

        const result = await repository.terminate(series.identifier).unwrap();

        expect(result).toBeUndefined();

        const findResult = await repository.find(series.identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(findResult).not.toBeNull();
        expect(isAggregateNotFoundError(findResult)).toBe(true);
      });

      it("存在しないシリーズを削除しようとするとエラーになる", async () => {
        const repository = FirebaseSeriesRepository(firestore, getOperations());
        const identifier = Builder(SeriesIdentifierFactory).buildWith(61);

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
