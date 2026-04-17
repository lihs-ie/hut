import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
  Timestamp,
} from "firebase/firestore";
import {
  createSlugIndexInTransaction,
  createVersion,
  deleteSlugIndexInTransaction,
  FIRESTORE_IN_BATCH_LIMIT,
  FirestoreOperations,
  mapFirestoreError,
  updateSlugIndexInTransaction,
  Version,
} from "./common";
import {
  Criteria,
  Memo,
  MemoIdentifier,
  MemoRepository,
  MemoSlug,
  validateMemo,
} from "@shared/domains/memo";
import { Order, SortByField } from "@shared/domains/common/sort";
import { fromPromise } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  duplicationError,
} from "@shared/aspects/error";
import { chunk } from "@shared/aspects/array";

type PersistedMemo = {
  identifier: string;
  title: string;
  slug: string;
  entries: Array<{
    text: string;
    createdAt: Timestamp;
  }>;
  tags: string[];
  images: string[];
  status: string;
  publishedAt: Timestamp | null;
  timeline: {
    createdAt: Timestamp;
    updatedAt: Timestamp;
  };
  version: number;
};

export const FirebaseMemoRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
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
          slug: memo.slug,
          entries: memo.entries.map((entry) => ({
            text: entry.text,
            createdAt: Timestamp.fromDate(entry.createdAt),
          })),
          tags: memo.tags,
          images: memo.images,
          status: memo.status,
          publishedAt: memo.publishedAt
            ? Timestamp.fromDate(memo.publishedAt)
            : null,
          timeline: {
            createdAt: Timestamp.fromDate(memo.timeline.createdAt),
            updatedAt: Timestamp.fromDate(memo.timeline.updatedAt),
          },
          version: currentVersion?.increment().value ?? 1,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedMemo>,
        options?: SnapshotOptions,
      ): Memo => {
        const data = snapshot.data(options);

        const memo = validateMemo({
          identifier: data.identifier,
          title: data.title,
          slug: data.slug,
          entries: data.entries.map((entry) => ({
            text: entry.text,
            createdAt: entry.createdAt.toDate(),
          })),
          tags: data.tags,
          images: data.images,
          status: data.status,
          publishedAt: data.publishedAt ? data.publishedAt.toDate() : null,
          timeline: {
            createdAt: data.timeline.createdAt.toDate(),
            updatedAt: data.timeline.updatedAt.toDate(),
          },
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
              `Memo ${memo.identifier} not found for update.`,
            );
          }

          const existingData = snapshot.data();
          const oldSlug = existingData.slug;

          await updateSlugIndexInTransaction(
            transaction,
            operations,
            firestore,
            {
              collectionName: "memos",
              oldSlug,
              newSlug: memo.slug,
              referenceIdentifier: memo.identifier,
              aggregateName: "Memo",
            },
          );

          transaction.set(document, memo, { merge: true });
          versions.set(memo.identifier, currentVersion.increment());
        } else {
          if (snapshot.exists()) {
            throw duplicationError("Memo", memo.identifier);
          }

          await createSlugIndexInTransaction(
            transaction,
            operations,
            firestore,
            {
              collectionName: "memos",
              slug: memo.slug,
              referenceIdentifier: memo.identifier,
              aggregateName: "Memo",
            },
          );

          transaction.set(document, memo);
          versions.set(memo.identifier, createVersion(1));
        }
      }),
      mapError,
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
      mapError,
    );
  };

  const search: MemoRepository["search"] = (criteria: Criteria) => {
    return fromPromise(
      (async () => {
        const constraints = [];

        if (criteria.status) {
          constraints.push(operations.where("status", "==", criteria.status));
        }

        if (criteria.tags && criteria.tags.length > 0) {
          constraints.push(
            operations.where("tags", "array-contains-any", criteria.tags),
          );
        }

        const sortByField = criteria.sortBy ?? SortByField.CREATED_AT;
        const sortOrder = criteria.order ?? Order.DESC;
        constraints.push(
          operations.orderBy(`timeline.${sortByField}`, sortOrder),
        );

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
                entry.text.toLowerCase().includes(keyword),
              ),
          );
        }

        return memos;
      })(),
      mapError,
    );
  };

  const terminate: MemoRepository["terminate"] = (
    identifier: MemoIdentifier,
  ) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, identifier);
        const snapshot = await transaction.get(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "Memo",
            `Memo ${identifier} not found for deletion.`,
          );
        }

        const memo = snapshot.data();
        deleteSlugIndexInTransaction(transaction, operations, firestore, {
          collectionName: "memos",
          slug: memo.slug,
        });

        transaction.delete(document);
        versions.delete(identifier);
      }),
      mapError,
    );
  };

  const findBySlug: MemoRepository["findBySlug"] = (slug: MemoSlug) => {
    return fromPromise(
      (async () => {
        const q = operations.query(
          collection,
          operations.where("slug", "==", slug),
        );
        const querySnapshot = await operations.getDocs(q);

        if (querySnapshot.empty) {
          throw aggregateNotFoundError(
            "Memo",
            `Memo with slug ${slug} not found.`,
          );
        }

        const doc = querySnapshot.docs[0];
        return doc.data();
      })(),
      mapError,
    );
  };

  const ofIdentifiers: MemoRepository["ofIdentifiers"] = (
    identifiers: MemoIdentifier[],
    throwOnMissing = false,
  ) => {
    return fromPromise(
      (async () => {
        if (identifiers.length === 0) {
          return [];
        }

        const uniqueIdentifiers = Array.from(new Set(identifiers));
        const chunks = chunk(uniqueIdentifiers, FIRESTORE_IN_BATCH_LIMIT).unwrap();

        const batchResults = await Promise.all(
          chunks.map(async (identifiersChunk) => {
            const query = operations.query(
              collection,
              operations.where("identifier", "in", identifiersChunk),
            );
            const snapshot = await operations.getDocs(query);
            return snapshot.docs.map((document) => document.data());
          }),
        );

        const memos = new Map<MemoIdentifier, Memo>();
        for (const memo of batchResults.flat()) {
          memos.set(memo.identifier, memo);
        }

        if (throwOnMissing) {
          const missingIdentifier = uniqueIdentifiers.find(
            (identifier) => !memos.has(identifier),
          );
          if (missingIdentifier !== undefined) {
            throw aggregateNotFoundError(
              "Memo",
              `Memo ${missingIdentifier} not found.`,
            );
          }
        }

        return identifiers.flatMap((identifier) => {
          const memo = memos.get(identifier);
          return memo !== undefined ? [memo] : [];
        });
      })(),
      mapError,
    );
  };

  return {
    persist,
    find,
    findBySlug,
    ofIdentifiers,
    search,
    terminate,
  };
};
