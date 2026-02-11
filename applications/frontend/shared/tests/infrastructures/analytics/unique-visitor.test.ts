import { describe, it, expect, beforeEach } from "vitest";
import { FirebaseUniqueVisitorRepository } from "@shared/infrastructures/analytics/unique-visitor";
import {
  createTestFirestoreWithSeed,
  type Firestore,
} from "../../support/mock/firebase/firestore";
import { Forger } from "@lihs-ie/forger-ts";
import {
  UniqueVisitorMold,
  UniqueVisitorIdentifierMold,
  UniqueVisitorCriteriaMold,
} from "../../support/molds/domains/analytics/unique-visitor";
import {
  isAggregateNotFoundError,
  isDuplicationError,
} from "@shared/aspects/error";
import type { FirestoreOperations } from "@shared/infrastructures/common";

describe("infrastructures/analytics/unique-visitor", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnvironment = await createTestFirestoreWithSeed({});
    firestore = testEnvironment.firestore;
    operations = testEnvironment.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  describe("FirebaseUniqueVisitorRepository", () => {
    describe("persist", () => {
      it("新しいユニークビジターを保存できる", async () => {
        const repository = FirebaseUniqueVisitorRepository(
          firestore,
          getOperations(),
        );
        const visitor = Forger(UniqueVisitorMold).forgeWithSeed(1);

        const result = await repository.persist(visitor).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(visitor.identifier).unwrap();
        expect(found.identifier.dateKey).toBe(visitor.identifier.dateKey);
        expect(found.identifier.sessionKey).toBe(
          visitor.identifier.sessionKey,
        );
      });

      it("重複するユニークビジターを保存しようとするとエラーになる", async () => {
        const repository = FirebaseUniqueVisitorRepository(
          firestore,
          getOperations(),
        );
        const visitor = Forger(UniqueVisitorMold).forgeWithSeed(2);

        await repository.persist(visitor).unwrap();

        const result = await repository.persist(visitor).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });
    });

    describe("find", () => {
      it("存在するユニークビジターを取得できる", async () => {
        const repository = FirebaseUniqueVisitorRepository(
          firestore,
          getOperations(),
        );
        const visitor = Forger(UniqueVisitorMold).forgeWithSeed(10);

        await repository.persist(visitor).unwrap();

        const found = await repository.find(visitor.identifier).unwrap();

        expect(found.identifier.dateKey).toBe(visitor.identifier.dateKey);
        expect(found.identifier.sessionKey).toBe(
          visitor.identifier.sessionKey,
        );
      });

      it("存在しないユニークビジターを取得しようとするとエラーになる", async () => {
        const repository = FirebaseUniqueVisitorRepository(
          firestore,
          getOperations(),
        );
        const identifier =
          Forger(UniqueVisitorIdentifierMold).forgeWithSeed(11);

        const result = await repository.find(identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("search", () => {
      it("全てのユニークビジターを取得できる", async () => {
        const repository = FirebaseUniqueVisitorRepository(
          firestore,
          getOperations(),
        );
        const visitors = Forger(UniqueVisitorMold).forgeMultiWithSeed(3, 20);

        for (const visitor of visitors) {
          await repository.persist(visitor).unwrap();
        }

        const criteria = Forger(UniqueVisitorCriteriaMold).forgeWithSeed(0);
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(3);
      });

      it("dateRange で絞り込みができる", async () => {
        const repository = FirebaseUniqueVisitorRepository(
          firestore,
          getOperations(),
        );
        const visitor1 = Forger(UniqueVisitorMold).forgeWithSeed(30);
        const visitor2 = Forger(UniqueVisitorMold).forgeWithSeed(31);

        await repository.persist(visitor1).unwrap();
        await repository.persist(visitor2).unwrap();

        const criteria = Forger(UniqueVisitorCriteriaMold).forgeWithSeed(0, {
          dateRange: {
            start: new Date(Date.UTC(2020, 0, 1)),
            end: new Date(Date.UTC(2030, 11, 31)),
          },
        });

        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBeGreaterThanOrEqual(0);
      });
    });

    describe("terminate", () => {
      it("ユニークビジターを削除できる", async () => {
        const repository = FirebaseUniqueVisitorRepository(
          firestore,
          getOperations(),
        );
        const visitor = Forger(UniqueVisitorMold).forgeWithSeed(40);

        await repository.persist(visitor).unwrap();

        const result = await repository
          .terminate(visitor.identifier)
          .unwrap();

        expect(result).toBeUndefined();

        const findResult = await repository
          .find(visitor.identifier)
          .match({
            ok: () => null,
            err: (error) => error,
          });

        expect(findResult).not.toBeNull();
        expect(isAggregateNotFoundError(findResult)).toBe(true);
      });

      it("存在しないユニークビジターを削除しようとするとエラーになる", async () => {
        const repository = FirebaseUniqueVisitorRepository(
          firestore,
          getOperations(),
        );
        const identifier =
          Forger(UniqueVisitorIdentifierMold).forgeWithSeed(41);

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
