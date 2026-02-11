import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
} from "firebase/firestore";
import { FirestoreOperations, mapFirestoreError } from "../common";
import {
  EngagementRecord,
  EngagementRecordRepository,
  validateEngagementRecord,
} from "@shared/domains/analytics/engagement";
import type { Criteria } from "@shared/domains/analytics/engagement";
import { fromPromise } from "@shared/aspects/result";
import { aggregateNotFoundError } from "@shared/aspects/error";
import {
  CONTENT_TYPES,
  resolveDateKeysFromCriteria,
  toCompositeDocumentId,
  parseCompositeDocumentId,
} from "./common";

type PersistedEngagementRecord = {
  dwellTime: number;
  maxScrollDepth: number;
  createdAt: FirebaseTimestamp;
  updatedAt: FirebaseTimestamp;
};

type FirebaseTimestamp = {
  toDate(): Date;
};

export const FirebaseEngagementRecordRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): EngagementRecordRepository => {
  const mapError = mapFirestoreError("EngagementRecord");

  const engagementRecordConverter = {
    toFirestore: (record: EngagementRecord): PersistedEngagementRecord => ({
      dwellTime: record.dwellTime,
      maxScrollDepth: record.scrollDepth,
      createdAt: operations.createTimestamp(record.createdAt),
      updatedAt: operations.createTimestamp(record.updatedAt),
    }),
    fromFirestore: (
      snapshot: QueryDocumentSnapshot<PersistedEngagementRecord>,
      options?: SnapshotOptions,
    ): EngagementRecord => {
      const data = snapshot.data(options);
      const pathSegments = snapshot.ref.path.split("/");
      const contentType = pathSegments[1];
      const contentId = pathSegments[2];
      const { dateKey, sessionKey } = parseCompositeDocumentId(
        pathSegments[3] ?? "",
      );

      return validateEngagementRecord({
        identifier: {
          reference: { type: contentType ?? "", content: contentId ?? "" },
          dateKey,
          sessionKey,
        },
        dwellTime: data.dwellTime,
        scrollDepth: data.maxScrollDepth,
        createdAt: data.createdAt.toDate(),
        updatedAt: data.updatedAt.toDate(),
      }).unwrap();
    },
  };

  const engagementCollectionOf = (contentType: string, contentId: string) =>
    operations
      .collection(firestore, "engagement-logs", contentType, contentId)
      .withConverter(engagementRecordConverter);

  const typeIndexDocumentOf = (contentType: string) =>
    operations.doc(firestore, "engagement-logs", contentType);

  const registerContentId = async (
    contentType: string,
    contentId: string,
  ): Promise<void> => {
    const indexDocument = typeIndexDocumentOf(contentType);
    const indexSnapshot = await operations.getDoc(indexDocument);
    const existingData = indexSnapshot.exists()
      ? (indexSnapshot.data() as Record<string, unknown>)
      : undefined;
    const existingIds = Array.isArray(existingData?.contentIds)
      ? (existingData.contentIds as string[])
      : [];

    if (!existingIds.includes(contentId)) {
      await operations.setDoc(
        indexDocument,
        { contentIds: [...existingIds, contentId] },
        { merge: true },
      );
    }
  };

  const resolveContentIds = async (
    contentType: string,
  ): Promise<string[]> => {
    const indexDocument = typeIndexDocumentOf(contentType);
    const indexSnapshot = await operations.getDoc(indexDocument);

    if (!indexSnapshot.exists()) {
      return [];
    }

    const data = indexSnapshot.data() as Record<string, unknown>;
    return Array.isArray(data?.contentIds)
      ? (data.contentIds as string[])
      : [];
  };

  const find: EngagementRecordRepository["find"] = (identifier) => {
    return fromPromise(
      (async () => {
        const collection = engagementCollectionOf(
          identifier.reference.type,
          identifier.reference.content,
        );
        const document = operations.doc(
          collection,
          toCompositeDocumentId(identifier),
        );
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "EngagementRecord",
            "EngagementRecord not found.",
          );
        }

        return snapshot.data();
      })(),
      mapError,
    );
  };

  const search: EngagementRecordRepository["search"] = (criteria: Criteria) => {
    return fromPromise(
      (async () => {
        const dateKeys = resolveDateKeysFromCriteria(criteria);
        const contentTypes = criteria.reference
          ? [criteria.reference.type]
          : CONTENT_TYPES;

        const typeResults = await Promise.all(
          contentTypes.map(async (contentType) => {
            const contentIds = await resolveContentIds(contentType);
            const filteredContentIds = criteria.reference
              ? contentIds.filter(
                  (contentId) => contentId === criteria.reference?.content,
                )
              : contentIds;

            const contentDocumentResults = await Promise.all(
              filteredContentIds.map(async (contentId) => {
                const collection = engagementCollectionOf(
                  contentType,
                  contentId,
                );
                const snapshot = await operations.getDocs(
                  operations.query(collection),
                );

                const records: EngagementRecord[] = [];
                snapshot.forEach((document) => {
                  const record = document.data();
                  if (dateKeys && !dateKeys.has(record.identifier.dateKey)) {
                    return;
                  }
                  records.push(record);
                });

                return records;
              }),
            );

            return contentDocumentResults.flat();
          }),
        );

        return typeResults.flat();
      })(),
      mapError,
    );
  };

  const persist: EngagementRecordRepository["persist"] = (record) => {
    return fromPromise(
      (async () => {
        await registerContentId(
          record.identifier.reference.type,
          record.identifier.reference.content,
        );
        const collection = engagementCollectionOf(
          record.identifier.reference.type,
          record.identifier.reference.content,
        );
        const document = operations.doc(
          collection,
          toCompositeDocumentId(record.identifier),
        );
        await operations.setDoc(document, record, { merge: true });
      })(),
      mapError,
    );
  };

  const terminate: EngagementRecordRepository["terminate"] = (identifier) => {
    return fromPromise(
      (async () => {
        const collection = engagementCollectionOf(
          identifier.reference.type,
          identifier.reference.content,
        );
        const document = operations.doc(
          collection,
          toCompositeDocumentId(identifier),
        );
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "EngagementRecord",
            "EngagementRecord not found.",
          );
        }

        await operations.deleteDoc(document);
      })(),
      mapError,
    );
  };

  return { find, search, persist, terminate };
};
