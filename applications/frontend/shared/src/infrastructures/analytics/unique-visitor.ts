import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
} from "firebase/firestore";
import { FirestoreOperations, mapFirestoreError } from "../common";
import {
  UniqueVisitor,
  UniqueVisitorIdentifier,
  UniqueVisitorRepository,
  validateUniqueVisitor,
} from "@shared/domains/analytics/unique-visitor";
import { fromPromise } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  duplicationError,
} from "@shared/aspects/error";
import {
  resolveDateKeysFromCriteria,
  toCompositeDocumentId,
  parseCompositeDocumentId,
  extractCountFromSnapshot,
} from "./common";

type FirebaseTimestamp = {
  toDate(): Date;
};

type PersistedUniqueVisitor = {
  createdAt: FirebaseTimestamp;
};

export const FirebaseUniqueVisitorRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): UniqueVisitorRepository => {
  const mapError = mapFirestoreError("UniqueVisitor");

  const uniqueVisitorConverter = {
    toFirestore: (visitor: UniqueVisitor): PersistedUniqueVisitor => ({
      createdAt: operations.createTimestamp(visitor.createdAt),
    }),
    fromFirestore: (
      snapshot: QueryDocumentSnapshot<PersistedUniqueVisitor>,
      options?: SnapshotOptions,
    ): UniqueVisitor => {
      const data = snapshot.data(options);
      const { dateKey, sessionKey } = parseCompositeDocumentId(snapshot.id);

      return validateUniqueVisitor({
        identifier: { dateKey, sessionKey },
        createdAt: data.createdAt.toDate(),
      }).unwrap();
    },
  };

  const dedupCollection = operations
    .collection(firestore, "unique-visitor-dedup")
    .withConverter(uniqueVisitorConverter);

  const toCounterPath = (
    identifier: UniqueVisitorIdentifier,
  ): [string, string] => ["unique-visitor-counters", identifier.dateKey];

  const find: UniqueVisitorRepository["find"] = (identifier) => {
    return fromPromise(
      (async () => {
        const document = operations.doc(
          dedupCollection,
          toCompositeDocumentId(identifier),
        );
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "UniqueVisitor",
            "UniqueVisitor not found.",
          );
        }

        return snapshot.data();
      })(),
      mapError,
    );
  };

  const search: UniqueVisitorRepository["search"] = (criteria) => {
    return fromPromise(
      (async () => {
        const dateKeys = resolveDateKeysFromCriteria(criteria);
        const snapshot = await operations.getDocs(dedupCollection);
        const results: UniqueVisitor[] = [];

        for (const document of snapshot.docs) {
          const dateKey = document.id.split(":")[0];
          if (dateKeys && !dateKeys.has(dateKey ?? "")) continue;

          results.push(document.data());
        }

        return results;
      })(),
      mapError,
    );
  };

  const persist: UniqueVisitorRepository["persist"] = (visitor) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const dedupDocument = operations.doc(
          dedupCollection,
          toCompositeDocumentId(visitor.identifier),
        );
        const counterDocument = operations.doc(
          firestore,
          ...toCounterPath(visitor.identifier),
        );

        const [dedupSnapshot, counterSnapshot] = await Promise.all([
          transaction.get(dedupDocument),
          transaction.get(counterDocument),
        ]);

        if (dedupSnapshot.exists()) {
          throw duplicationError(
            "UniqueVisitor",
            "UniqueVisitor already exists.",
          );
        }

        const currentCount = extractCountFromSnapshot(counterSnapshot.data());

        transaction.set(dedupDocument, visitor);
        transaction.set(
          counterDocument,
          {
            count: currentCount + 1,
            updatedAt: operations.createTimestamp(visitor.createdAt),
          },
          { merge: true },
        );
      }),
      mapError,
    );
  };

  const terminate: UniqueVisitorRepository["terminate"] = (identifier) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const dedupDocument = operations.doc(
          dedupCollection,
          toCompositeDocumentId(identifier),
        );
        const counterDocument = operations.doc(
          firestore,
          ...toCounterPath(identifier),
        );

        const [dedupSnapshot, counterSnapshot] = await Promise.all([
          transaction.get(dedupDocument),
          transaction.get(counterDocument),
        ]);

        if (!dedupSnapshot.exists()) {
          throw aggregateNotFoundError(
            "UniqueVisitor",
            "UniqueVisitor not found.",
          );
        }

        const currentCount = extractCountFromSnapshot(counterSnapshot.data());

        if (currentCount > 0) {
          transaction.set(
            counterDocument,
            {
              count: currentCount - 1,
              updatedAt: operations.createTimestamp(new Date()),
            },
            { merge: true },
          );
        }

        transaction.delete(dedupDocument);
      }),
      mapError,
    );
  };

  return { find, search, persist, terminate };
};
