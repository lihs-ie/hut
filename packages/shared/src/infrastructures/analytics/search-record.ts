import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
} from "firebase/firestore";
import { FirestoreOperations, mapFirestoreError } from "../common";
import {
  SearchRecord,
  SearchRecordRepository,
  validateSearchRecord,
} from "@shared/domains/analytics/search-record";
import type { Criteria } from "@shared/domains/analytics/search-record";
import { toJstDateKey } from "@shared/domains/analytics/common";
import { fromPromise } from "@shared/aspects/result";
import { aggregateNotFoundError } from "@shared/aspects/error";
import { resolveDateKeysFromCriteria } from "./common";
import { decodeTime } from "ulid";

type FirebaseTimestamp = {
  toDate(): Date;
};

type PersistedSearchRecord = {
  keyword: string;
  resultCount: number;
  tags: string[] | null;
  type: string | null;
  createdAt: FirebaseTimestamp;
};

const extractDateKeyFromUlid = (ulidValue: string): string =>
  toJstDateKey(new Date(decodeTime(ulidValue)));

export const FirebaseSearchRecordRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): SearchRecordRepository => {
  const mapError = mapFirestoreError("SearchRecord");

  const searchRecordConverter = {
    toFirestore: (record: SearchRecord): PersistedSearchRecord => ({
      keyword: record.keyword,
      resultCount: record.resultCount,
      tags: record.tags ? [...record.tags] : null,
      type: record.contentType,
      createdAt: operations.createTimestamp(record.createdAt),
    }),
    fromFirestore: (
      snapshot: QueryDocumentSnapshot<PersistedSearchRecord>,
      options?: SnapshotOptions,
    ): SearchRecord => {
      const data = snapshot.data(options);
      const pathSegments = snapshot.ref.path.split("/");
      const dateKey = pathSegments[1];

      return validateSearchRecord({
        identifier: snapshot.id,
        dateKey: dateKey ?? "",
        keyword: data.keyword,
        resultCount: data.resultCount,
        tags: data.tags,
        contentType: data.type,
        createdAt: data.createdAt.toDate(),
      }).unwrap();
    },
  };

  const searchLogCollectionOf = (dateKey: string) =>
    operations
      .collection(firestore, "search-logs", dateKey, "records")
      .withConverter(searchRecordConverter);

  const find: SearchRecordRepository["find"] = (identifier) => {
    return fromPromise(
      (async () => {
        const dateKey = extractDateKeyFromUlid(identifier);
        const collection = searchLogCollectionOf(dateKey);
        const document = operations.doc(collection, identifier);
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "SearchRecord",
            "SearchRecord not found.",
          );
        }

        return snapshot.data();
      })(),
      mapError,
    );
  };

  const search: SearchRecordRepository["search"] = (criteria: Criteria) => {
    return fromPromise(
      (async () => {
        const dateKeys = resolveDateKeysFromCriteria(criteria);

        if (!dateKeys) {
          return [];
        }

        const results = await Promise.all(
          Array.from(dateKeys).map(async (dateKey) => {
            const collection = searchLogCollectionOf(dateKey);
            const querySnapshot = await operations.getDocs(collection);

            const records: SearchRecord[] = [];
            querySnapshot.forEach((document) => {
              records.push(document.data());
            });

            return records;
          }),
        );

        return results.flat().filter((record) => {
          if (criteria.keyword && !record.keyword.includes(criteria.keyword)) {
            return false;
          }

          if (criteria.hasResults === true && record.resultCount === 0) {
            return false;
          }

          if (criteria.hasResults === false && record.resultCount > 0) {
            return false;
          }

          return true;
        });
      })(),
      mapError,
    );
  };

  const persist: SearchRecordRepository["persist"] = (record) => {
    return fromPromise(
      (async () => {
        const collection = searchLogCollectionOf(record.dateKey);
        const document = operations.doc(collection, record.identifier);
        await operations.setDoc(document, record);
      })(),
      mapError,
    );
  };

  const terminate: SearchRecordRepository["terminate"] = (identifier) => {
    return fromPromise(
      (async () => {
        const dateKey = extractDateKeyFromUlid(identifier);
        const collection = searchLogCollectionOf(dateKey);
        const document = operations.doc(collection, identifier);
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "SearchRecord",
            "SearchRecord not found.",
          );
        }

        await operations.deleteDoc(document);
      })(),
      mapError,
    );
  };

  return { find, search, persist, terminate };
};
