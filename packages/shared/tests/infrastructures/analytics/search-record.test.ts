import { describe, it, expect, beforeEach } from "vitest";
import { FirebaseSearchRecordRepository } from "@shared/infrastructures/analytics/search-record";
import {
  createTestFirestoreWithSeed,
  type Firestore,
} from "../../support/mock/firebase/firestore";
import { Forger } from "@lihs-ie/forger-ts";
import {
  SearchRecordMold,
  SearchRecordIdentifierMold,
  CriteriaMold,
} from "../../support/molds/domains/analytics/search-record";
import { isAggregateNotFoundError } from "@shared/aspects/error";
import { toJstDateKey } from "@shared/domains/analytics/common";
import type {
  SearchRecord,
  Criteria,
} from "@shared/domains/analytics/search-record";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { decodeTime } from "ulid";

describe("infrastructures/analytics/search-record", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnvironment = await createTestFirestoreWithSeed({});
    firestore = testEnvironment.firestore;
    operations = testEnvironment.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  const buildSearchRecord = (seed: number, overrides?: Partial<Parameters<typeof SearchRecordMold.pour>[0]>): SearchRecord => {
    const identifier = Forger(SearchRecordIdentifierMold).forgeWithSeed(seed);
    const timestamp = new Date(decodeTime(identifier));
    const dateKey = toJstDateKey(timestamp);

    return Forger(SearchRecordMold).forgeWithSeed(seed, {
      identifier,
      dateKey,
      createdAt: timestamp,
      ...overrides,
    });
  };

  const buildDateRangeAroundRecord = (record: SearchRecord) => {
    const recordDate = record.createdAt;
    const start = new Date(recordDate.getTime() - 24 * 60 * 60 * 1000);
    const end = new Date(recordDate.getTime() + 24 * 60 * 60 * 1000);
    return { start, end };
  };

  const buildCriteria = (overrides?: Partial<Parameters<typeof CriteriaMold.pour>[0]>): Criteria => {
    return Forger(CriteriaMold).forgeWithSeed(1, {
      dateRange: overrides?.dateRange === undefined
        ? { start: new Date("2020-01-01"), end: new Date("2030-01-01") }
        : overrides.dateRange,
      keyword: overrides?.keyword === undefined ? null : overrides.keyword,
      hasResults: overrides?.hasResults === undefined ? null : overrides.hasResults,
    });
  };

  describe("FirebaseSearchRecordRepository", () => {
    describe("persist", () => {
      it("新しい検索記録を保存できる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const record = buildSearchRecord(1);

        const result = await repository.persist(record).unwrap();

        expect(result).toBeUndefined();
      });

      it("保存した検索記録をfindで取得できる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const record = buildSearchRecord(2);

        await repository.persist(record).unwrap();

        const found = await repository.find(record.identifier).unwrap();
        expect(found.identifier).toBe(record.identifier);
        expect(found.keyword).toBe(record.keyword);
        expect(found.resultCount).toBe(record.resultCount);
      });

      it("contentTypeを保存できる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const record = buildSearchRecord(3);

        await repository.persist(record).unwrap();

        const found = await repository.find(record.identifier).unwrap();
        expect(found.contentType).toBe(record.contentType);
      });

      it("tagsを保存できる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const record = buildSearchRecord(5);

        await repository.persist(record).unwrap();

        const found = await repository.find(record.identifier).unwrap();
        if (record.tags === null) {
          expect(found.tags).toBeNull();
        } else {
          expect(found.tags).toEqual(record.tags);
        }
      });
    });

    describe("find", () => {
      it("存在する検索記録を取得できる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const record = buildSearchRecord(10);

        await repository.persist(record).unwrap();

        const found = await repository.find(record.identifier).unwrap();
        expect(found.identifier).toBe(record.identifier);
        expect(found.keyword).toBe(record.keyword);
        expect(found.resultCount).toBe(record.resultCount);
        expect(found.dateKey).toBe(record.dateKey);
      });

      it("存在しない検索記録を取得しようとするとエラーになる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const identifier = Forger(SearchRecordIdentifierMold).forgeWithSeed(11);

        const result = await repository.find(identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("search", () => {
      it("dateRange未指定の場合は空配列を返す", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const record = buildSearchRecord(20);
        await repository.persist(record).unwrap();

        const criteria = buildCriteria({ dateRange: null });
        const found = await repository.search(criteria).unwrap();

        expect(found).toEqual([]);
      });

      it("dateRangeで検索記録を取得できる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const record = buildSearchRecord(21);
        await repository.persist(record).unwrap();

        const criteria = buildCriteria({
          dateRange: buildDateRangeAroundRecord(record),
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBeGreaterThanOrEqual(1);
        expect(found.some((r) => r.identifier === record.identifier)).toBe(true);
      });

      it("複数の検索記録を取得できる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const fixedTimestamp = new Date("2025-06-15T12:00:00Z");
        const fixedDateKey = toJstDateKey(fixedTimestamp);

        const record1 = buildSearchRecord(30, {
          dateKey: fixedDateKey,
          createdAt: fixedTimestamp,
        });
        const record2 = buildSearchRecord(31, {
          dateKey: fixedDateKey,
          createdAt: fixedTimestamp,
        });

        await repository.persist(record1).unwrap();
        await repository.persist(record2).unwrap();

        const criteria = buildCriteria({
          dateRange: {
            start: new Date("2025-06-14"),
            end: new Date("2025-06-16"),
          },
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBeGreaterThanOrEqual(2);
      });

      it("hasResultsでフィルタリングできる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const fixedTimestamp = new Date("2025-06-15T12:00:00Z");
        const fixedDateKey = toJstDateKey(fixedTimestamp);

        const recordWithResults = buildSearchRecord(40, {
          resultCount: 5,
          dateKey: fixedDateKey,
          createdAt: fixedTimestamp,
        });
        const recordWithoutResults = buildSearchRecord(41, {
          resultCount: 0,
          dateKey: fixedDateKey,
          createdAt: fixedTimestamp,
        });

        await repository.persist(recordWithResults).unwrap();
        await repository.persist(recordWithoutResults).unwrap();

        const dateRange = {
          start: new Date("2025-06-14"),
          end: new Date("2025-06-16"),
        };

        const criteriaWithResults = buildCriteria({
          dateRange,
          hasResults: true,
        });
        const foundWithResults = await repository.search(criteriaWithResults).unwrap();

        const allHaveResults = foundWithResults.every((r) => r.resultCount > 0);
        expect(allHaveResults).toBe(true);
        expect(foundWithResults.length).toBeGreaterThanOrEqual(1);

        const criteriaWithoutResults = buildCriteria({
          dateRange,
          hasResults: false,
        });
        const foundWithoutResults = await repository.search(criteriaWithoutResults).unwrap();

        const allHaveNoResults = foundWithoutResults.every((r) => r.resultCount === 0);
        expect(allHaveNoResults).toBe(true);
        expect(foundWithoutResults.length).toBeGreaterThanOrEqual(1);
      });
    });

    describe("terminate", () => {
      it("検索記録を削除できる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const record = buildSearchRecord(50);

        await repository.persist(record).unwrap();

        const result = await repository.terminate(record.identifier).unwrap();

        expect(result).toBeUndefined();

        const findResult = await repository.find(record.identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(findResult).not.toBeNull();
        expect(isAggregateNotFoundError(findResult)).toBe(true);
      });

      it("存在しない検索記録を削除しようとするとエラーになる", async () => {
        const repository = FirebaseSearchRecordRepository(
          firestore,
          getOperations(),
        );
        const identifier = Forger(SearchRecordIdentifierMold).forgeWithSeed(51);

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
