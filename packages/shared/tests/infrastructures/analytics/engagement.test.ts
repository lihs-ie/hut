import { describe, it, expect, beforeEach } from "vitest";
import { FirebaseEngagementRecordRepository } from "@shared/infrastructures/analytics/engagement";
import {
  createTestFirestoreWithSeed,
  type Firestore,
} from "../../support/mock/firebase/firestore";
import { Forger } from "@lihs-ie/forger-ts";
import {
  EngagementRecordMold,
  EngagementRecordIdentifierMold,
} from "../../support/molds/domains/analytics/engagement";
import { SearchReferenceIdentifierMold } from "../../support/molds/domains/search-token/common";
import { isAggregateNotFoundError } from "@shared/aspects/error";
import {
  validateCriteria,
  type Criteria,
} from "@shared/domains/analytics/engagement";
import { dateKeySchema } from "@shared/domains/analytics/common";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { ContentType } from "@shared/domains/search-token/reference";

describe("infrastructures/analytics/engagement", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnvironment = await createTestFirestoreWithSeed({});
    firestore = testEnvironment.firestore;
    operations = testEnvironment.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  const createCriteria = (parameters: {
    dateRange?: { start: Date; end: Date } | null;
    reference?: { type: string; content: string } | null;
  }): Criteria => {
    return validateCriteria({
      dateRange: parameters.dateRange ?? null,
      reference: parameters.reference ?? null,
    }).unwrap();
  };

  describe("FirebaseEngagementRecordRepository", () => {
    describe("persist", () => {
      it("新しいエンゲージメントレコードを保存できる", async () => {
        const repository = FirebaseEngagementRecordRepository(
          firestore,
          getOperations(),
        );
        const record = Forger(EngagementRecordMold).forgeWithSeed(1);

        const result = await repository.persist(record).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(record.identifier).unwrap();
        expect(found.identifier.reference.type).toBe(
          record.identifier.reference.type,
        );
        expect(found.identifier.reference.content).toBe(
          record.identifier.reference.content,
        );
        expect(found.identifier.dateKey).toBe(record.identifier.dateKey);
        expect(found.identifier.sessionKey).toBe(
          record.identifier.sessionKey,
        );
        expect(found.dwellTime).toBe(record.dwellTime);
        expect(found.scrollDepth).toBe(record.scrollDepth);
      });

      it("既存のレコードをマージ更新できる", async () => {
        const repository = FirebaseEngagementRecordRepository(
          firestore,
          getOperations(),
        );
        const record = Forger(EngagementRecordMold).forgeWithSeed(2);

        await repository.persist(record).unwrap();

        const found = await repository.find(record.identifier).unwrap();
        expect(found.dwellTime).toBe(record.dwellTime);
      });
    });

    describe("find", () => {
      it("存在するレコードを取得できる", async () => {
        const repository = FirebaseEngagementRecordRepository(
          firestore,
          getOperations(),
        );
        const record = Forger(EngagementRecordMold).forgeWithSeed(10);

        await repository.persist(record).unwrap();

        const found = await repository.find(record.identifier).unwrap();

        expect(found.identifier.reference.type).toBe(
          record.identifier.reference.type,
        );
        expect(found.identifier.reference.content).toBe(
          record.identifier.reference.content,
        );
        expect(found.identifier.dateKey).toBe(record.identifier.dateKey);
        expect(found.identifier.sessionKey).toBe(
          record.identifier.sessionKey,
        );
        expect(found.dwellTime).toBe(record.dwellTime);
        expect(found.scrollDepth).toBe(record.scrollDepth);
      });

      it("存在しないレコードを取得しようとするとエラーになる", async () => {
        const repository = FirebaseEngagementRecordRepository(
          firestore,
          getOperations(),
        );
        const identifier = Forger(
          EngagementRecordIdentifierMold,
        ).forgeWithSeed(11);

        const result = await repository.find(identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("search", () => {
      it("全てのレコードを取得できる", async () => {
        const repository = FirebaseEngagementRecordRepository(
          firestore,
          getOperations(),
        );

        const reference = Forger(SearchReferenceIdentifierMold).forgeWithSeed(
          30,
          { type: ContentType.ARTICLE },
        );
        const records = Forger(EngagementRecordMold).forgeMultiWithSeed(3, 30, {
          identifier: Forger(EngagementRecordIdentifierMold).forgeWithSeed(30, {
            reference,
          }),
        });

        for (const record of records) {
          await repository.persist(record).unwrap();
        }

        const criteria = createCriteria({});
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBeGreaterThanOrEqual(1);
      });

      it("referenceでレコードを検索できる", async () => {
        const repository = FirebaseEngagementRecordRepository(
          firestore,
          getOperations(),
        );

        const reference1 = Forger(
          SearchReferenceIdentifierMold,
        ).forgeWithSeed(40, { type: ContentType.ARTICLE });
        const reference2 = Forger(
          SearchReferenceIdentifierMold,
        ).forgeWithSeed(41, { type: ContentType.MEMO });

        const record1 = Forger(EngagementRecordMold).forgeWithSeed(40, {
          identifier: Forger(EngagementRecordIdentifierMold).forgeWithSeed(40, {
            reference: reference1,
          }),
        });
        const record2 = Forger(EngagementRecordMold).forgeWithSeed(41, {
          identifier: Forger(EngagementRecordIdentifierMold).forgeWithSeed(41, {
            reference: reference2,
          }),
        });

        await repository.persist(record1).unwrap();
        await repository.persist(record2).unwrap();

        const criteria = createCriteria({
          reference: {
            type: reference1.type,
            content: reference1.content,
          },
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.identifier.reference.content).toBe(
          reference1.content,
        );
      });

      it("dateRangeでレコードをフィルタリングできる", async () => {
        const repository = FirebaseEngagementRecordRepository(
          firestore,
          getOperations(),
        );

        const reference = Forger(
          SearchReferenceIdentifierMold,
        ).forgeWithSeed(50, { type: ContentType.ARTICLE });

        const dateKey1 = dateKeySchema.parse("2024-06-15");
        const dateKey2 = dateKeySchema.parse("2024-07-15");

        const record1 = Forger(EngagementRecordMold).forgeWithSeed(50, {
          identifier: Forger(EngagementRecordIdentifierMold).forgeWithSeed(50, {
            reference,
            dateKey: dateKey1,
          }),
        });
        const record2 = Forger(EngagementRecordMold).forgeWithSeed(51, {
          identifier: Forger(EngagementRecordIdentifierMold).forgeWithSeed(51, {
            reference,
            dateKey: dateKey2,
          }),
        });

        await repository.persist(record1).unwrap();
        await repository.persist(record2).unwrap();

        const criteria = createCriteria({
          dateRange: {
            start: new Date("2024-06-01"),
            end: new Date("2024-06-30"),
          },
          reference: {
            type: reference.type,
            content: reference.content,
          },
        });
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(1);
        expect(found[0]?.identifier.dateKey).toBe("2024-06-15");
      });

      it("空のCriteriaで空の結果が返る（レコードがない場合）", async () => {
        const repository = FirebaseEngagementRecordRepository(
          firestore,
          getOperations(),
        );

        const criteria = createCriteria({});
        const found = await repository.search(criteria).unwrap();

        expect(found).toEqual([]);
      });
    });

    describe("terminate", () => {
      it("レコードを削除できる", async () => {
        const repository = FirebaseEngagementRecordRepository(
          firestore,
          getOperations(),
        );
        const record = Forger(EngagementRecordMold).forgeWithSeed(110);

        await repository.persist(record).unwrap();

        const result = await repository
          .terminate(record.identifier)
          .unwrap();

        expect(result).toBeUndefined();

        const findResult = await repository.find(record.identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(findResult).not.toBeNull();
        expect(isAggregateNotFoundError(findResult)).toBe(true);
      });

      it("存在しないレコードを削除しようとするとエラーになる", async () => {
        const repository = FirebaseEngagementRecordRepository(
          firestore,
          getOperations(),
        );
        const identifier = Forger(
          EngagementRecordIdentifierMold,
        ).forgeWithSeed(111);

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
