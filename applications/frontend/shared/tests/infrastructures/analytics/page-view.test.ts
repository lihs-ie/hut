import { describe, it, expect, beforeEach } from "vitest";
import { FirebasePageViewRepository } from "@shared/infrastructures/analytics/page-view";
import {
  createTestFirestoreWithSeed,
  type Firestore,
} from "../../support/mock/firebase/firestore";
import { Forger } from "@lihs-ie/forger-ts";
import {
  PageViewMold,
  PageViewIdentifierMold,
  PageViewCriteriaMold,
} from "../../support/molds/domains/analytics/page-view";
import {
  isAggregateNotFoundError,
  isDuplicationError,
} from "@shared/aspects/error";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { dateRangeSchema } from "@shared/domains/analytics/common";

describe("infrastructures/analytics/page-view", () => {
  let firestore: Firestore;
  let operations: unknown;

  beforeEach(async () => {
    const testEnvironment = await createTestFirestoreWithSeed({});
    firestore = testEnvironment.firestore;
    operations = testEnvironment.operations;
  });

  const getOperations = () => operations as FirestoreOperations;

  describe("FirebasePageViewRepository", () => {
    describe("persist", () => {
      it("新しいページビューを保存できる", async () => {
        const repository = FirebasePageViewRepository(
          firestore,
          getOperations(),
        );
        const pageView = Forger(PageViewMold).forgeWithSeed(1);

        const result = await repository.persist(pageView).unwrap();

        expect(result).toBeUndefined();

        const found = await repository.find(pageView.identifier).unwrap();
        expect(found.identifier.dateKey).toBe(pageView.identifier.dateKey);
        expect(found.identifier.sessionKey).toBe(
          pageView.identifier.sessionKey,
        );
        expect(found.deviceType).toBe(pageView.deviceType);
      });

      it("重複するページビューを保存しようとするとエラーになる", async () => {
        const repository = FirebasePageViewRepository(
          firestore,
          getOperations(),
        );
        const pageView = Forger(PageViewMold).forgeWithSeed(2);

        await repository.persist(pageView).unwrap();

        const result = await repository.persist(pageView).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isDuplicationError(result)).toBe(true);
      });
    });

    describe("find", () => {
      it("存在するページビューを取得できる", async () => {
        const repository = FirebasePageViewRepository(
          firestore,
          getOperations(),
        );
        const pageView = Forger(PageViewMold).forgeWithSeed(10);

        await repository.persist(pageView).unwrap();

        const found = await repository.find(pageView.identifier).unwrap();

        expect(found.identifier.dateKey).toBe(pageView.identifier.dateKey);
        expect(found.identifier.sessionKey).toBe(
          pageView.identifier.sessionKey,
        );
        expect(found.identifier.reference.type).toBe(
          pageView.identifier.reference.type,
        );
        expect(found.identifier.reference.content).toBe(
          pageView.identifier.reference.content,
        );
        expect(found.referrer.raw).toBe(pageView.referrer.raw);
        expect(found.deviceType).toBe(pageView.deviceType);
      });

      it("存在しないページビューを取得しようとするとエラーになる", async () => {
        const repository = FirebasePageViewRepository(
          firestore,
          getOperations(),
        );
        const identifier = Forger(PageViewIdentifierMold).forgeWithSeed(11);

        const result = await repository.find(identifier).match({
          ok: () => null,
          err: (error) => error,
        });

        expect(result).not.toBeNull();
        expect(isAggregateNotFoundError(result)).toBe(true);
      });
    });

    describe("search", () => {
      it("全てのページビューを取得できる", async () => {
        const repository = FirebasePageViewRepository(
          firestore,
          getOperations(),
        );
        const pageViews = Forger(PageViewMold).forgeMultiWithSeed(3, 20);

        for (const pageView of pageViews) {
          await repository.persist(pageView).unwrap();
        }

        const criteria = Forger(PageViewCriteriaMold).forgeWithSeed(0);
        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBe(3);
      });

      it("dateRange で絞り込みができる", async () => {
        const repository = FirebasePageViewRepository(
          firestore,
          getOperations(),
        );
        const pageView1 = Forger(PageViewMold).forgeWithSeed(30);
        const pageView2 = Forger(PageViewMold).forgeWithSeed(31);

        await repository.persist(pageView1).unwrap();
        await repository.persist(pageView2).unwrap();

        const criteria = Forger(PageViewCriteriaMold).forgeWithSeed(0, {
          dateRange: dateRangeSchema.parse({
            start: new Date(Date.UTC(2020, 0, 1)),
            end: new Date(Date.UTC(2030, 11, 31)),
          }),
        });

        const found = await repository.search(criteria).unwrap();

        expect(found.length).toBeGreaterThanOrEqual(0);
      });
    });

    describe("terminate", () => {
      it("ページビューを削除できる", async () => {
        const repository = FirebasePageViewRepository(
          firestore,
          getOperations(),
        );
        const pageView = Forger(PageViewMold).forgeWithSeed(40);

        await repository.persist(pageView).unwrap();

        const result = await repository
          .terminate(pageView.identifier)
          .unwrap();

        expect(result).toBeUndefined();

        const findResult = await repository
          .find(pageView.identifier)
          .match({
            ok: () => null,
            err: (error) => error,
          });

        expect(findResult).not.toBeNull();
        expect(isAggregateNotFoundError(findResult)).toBe(true);
      });

      it("存在しないページビューを削除しようとするとエラーになる", async () => {
        const repository = FirebasePageViewRepository(
          firestore,
          getOperations(),
        );
        const identifier = Forger(PageViewIdentifierMold).forgeWithSeed(41);

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
