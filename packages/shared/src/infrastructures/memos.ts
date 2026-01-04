import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
} from "firebase/firestore";
import {
  createVersion,
  FirestoreOperations,
  mapFirestoreError,
  Version,
} from "./common";
import {
  Criteria,
  Memo,
  MemoIdentifier,
  MemoRepository,
  validateMemo,
} from "@/domains/memo";
import { fromPromise } from "@/aspects/result";
import { aggregateNotFoundError, duplicationError } from "@/aspects/error";

type PersistedMemo = {
  identifier: string;
  title: string;
  entries: Array<{
    text: string;
    createdAt: Date;
  }>;
  tags: string[];
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
  version: number;
};

export const FirebaseMemoRepository = (
  firestore: Firestore,
  operations: FirestoreOperations
): MemoRepository => {
  const mapError = mapFirestoreError("Memo");
  const versions: Map<MemoIdentifier, Version> = new Map();

  const collection = operations
    .collection(firestore, "memos")
    .withConverter<Memo, PersistedMemo>({
      toFirestore: (memo: Memo): PersistedMemo => {
        const currentVersion = versions.get(memo.identifier);

        return {
          identifier: memo.identifier,
          title: memo.title,
          entries: memo.entries.map((entry) => ({
            text: entry.text,
            createdAt: entry.createdAt,
          })),
          tags: [...memo.tags],
          timeline: {
            createdAt: memo.timeline.createdAt,
            updatedAt: memo.timeline.updatedAt,
          },
          version: currentVersion?.increment().value ?? 1,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedMemo>,
        options?: SnapshotOptions
      ): Memo => {
        const data = snapshot.data(options);

        const memo = validateMemo({
          identifier: data.identifier,
          title: data.title,
          entries: data.entries,
          tags: data.tags,
        }).unwrap();

        const version = createVersion(data.version);

        versions.set(memo.identifier, version);

        return memo;
      },
    });

  const persist: MemoRepository["persist"] = (memo: Memo) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, memo.identifier);
        const snapshot = await transaction.get(document);
        const currentVersion = versions.get(memo.identifier);

        if (currentVersion && currentVersion.value > 0) {
          if (!snapshot.exists()) {
            throw aggregateNotFoundError(
              "Memo",
              `Memo ${memo.identifier} not found for update.`
            );
          }

          transaction.set(document, memo, { merge: true });
        } else {
          if (snapshot.exists()) {
            throw duplicationError("Memo", memo.identifier);
          }

          transaction.set(document, memo);
        }
      }),
      mapError
    );
  };

  const find: MemoRepository["find"] = (identifier: MemoIdentifier) => {
    return fromPromise(
      (async () => {
        const document = operations.doc(collection, identifier);
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError("Memo", `Memo ${identifier} not found.`);
        }

        return snapshot.data();
      })(),
      mapError
    );
  };

  const search: MemoRepository["search"] = (criteria: Criteria) => {
    return fromPromise(
      (async () => {
        const constraints = [];

        if (criteria.tag) {
          constraints.push(
            operations.where("tags", "array-contains", criteria.tag)
          );
        }

        const q = operations.query(collection, ...constraints);
        const querySnapshot = await operations.getDocs(q);

        const memos: Memo[] = [];
        querySnapshot.forEach((doc) => {
          memos.push(doc.data());
        });

        if (criteria.freeWord) {
          const keyword = criteria.freeWord.toLowerCase();
          return memos.filter(
            (memo) =>
              memo.title.toLowerCase().includes(keyword) ||
              memo.entries.some((entry) =>
                entry.text.toLowerCase().includes(keyword)
              )
          );
        }

        return memos;
      })(),
      mapError
    );
  };

  const terminate: MemoRepository["terminate"] = (
    identifier: MemoIdentifier
  ) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, identifier);
        const snapshot = await transaction.get(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "Memo",
            `Memo ${identifier} not found for deletion.`
          );
        }

        transaction.delete(document);
      }),
      mapError
    );
  };

  return {
    persist,
    find,
    search,
    terminate,
  };
};
