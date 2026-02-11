import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
} from "firebase/firestore";
import { FirestoreOperations, mapFirestoreError } from "../common";
import {
  PageView,
  PageViewIdentifier,
  PageViewRepository,
  validatePageView,
} from "@shared/domains/analytics/page-view";
import { fromPromise } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  duplicationError,
} from "@shared/aspects/error";
import {
  CONTENT_TYPES,
  resolveDateKeysFromCriteria,
  toCompositeDocumentId,
  parseCompositeDocumentId,
  extractCountFromSnapshot,
} from "./common";

type FirebaseTimestamp = {
  toDate(): Date;
};

type PersistedAccessLog = {
  referrer: string | null;
  deviceType: string;
  sessionKey: string;
  createdAt: FirebaseTimestamp;
};

const toDedupPath = (
  identifier: PageViewIdentifier,
): [string, string, string, string] => [
  "page-view-dedup",
  identifier.reference.type,
  identifier.reference.content,
  toCompositeDocumentId(identifier),
];

const toCounterPath = (
  identifier: PageViewIdentifier,
): [string, string, string, string] => [
  "page-view-counters",
  identifier.reference.type,
  identifier.reference.content,
  identifier.dateKey,
];

export const FirebasePageViewRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): PageViewRepository => {
  const mapError = mapFirestoreError("PageView");

  const typeIndexDocumentOf = (contentType: string) =>
    operations.doc(firestore, "access-logs", contentType);

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

  const accessLogConverter = {
    toFirestore: (pageView: PageView): PersistedAccessLog => ({
      referrer: pageView.referrer.raw,
      deviceType: pageView.deviceType,
      sessionKey: pageView.identifier.sessionKey,
      createdAt: operations.createTimestamp(pageView.createdAt),
    }),
    fromFirestore: (
      snapshot: QueryDocumentSnapshot<PersistedAccessLog>,
      options?: SnapshotOptions,
    ): PageView => {
      const data = snapshot.data(options);
      const pathSegments = snapshot.ref.path.split("/");
      const contentType = pathSegments[1];
      const contentId = pathSegments[2];
      const { dateKey, sessionKey } = parseCompositeDocumentId(
        pathSegments[3] ?? "",
      );

      return validatePageView({
        identifier: {
          reference: { type: contentType ?? "", content: contentId ?? "" },
          dateKey,
          sessionKey,
        },
        referrer: { raw: data.referrer },
        deviceType: data.deviceType,
        createdAt: data.createdAt.toDate(),
      }).unwrap();
    },
  };

  const accessLogCollectionOf = (contentType: string, contentId: string) =>
    operations
      .collection(firestore, "access-logs", contentType, contentId)
      .withConverter(accessLogConverter);

  const find: PageViewRepository["find"] = (identifier) => {
    return fromPromise(
      (async () => {
        const accessLogCollection = accessLogCollectionOf(
          identifier.reference.type,
          identifier.reference.content,
        );
        const document = operations.doc(
          accessLogCollection,
          toCompositeDocumentId(identifier),
        );
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError("PageView", "PageView not found.");
        }

        return snapshot.data();
      })(),
      mapError,
    );
  };

  const search: PageViewRepository["search"] = (criteria) => {
    return fromPromise(
      (async () => {
        const dateKeys = resolveDateKeysFromCriteria(criteria);
        const contentTypes = criteria.reference
          ? [criteria.reference.type]
          : CONTENT_TYPES;

        const typeResults = await Promise.all(
          contentTypes.map(async (contentType) => {
            const contentIds = criteria.reference
              ? [criteria.reference.content]
              : await resolveContentIds(contentType);

            const contentDocumentResults = await Promise.all(
              contentIds.map(async (contentId) => {
                const accessLogCollection = accessLogCollectionOf(
                  contentType,
                  contentId,
                );
                const accessLogSnapshot =
                  await operations.getDocs(accessLogCollection);

                const records: PageView[] = [];
                for (const document of accessLogSnapshot.docs) {
                  const pageView = document.data();

                  if (dateKeys && !dateKeys.has(pageView.identifier.dateKey)) {
                    continue;
                  }

                  if (
                    criteria.deviceType &&
                    pageView.deviceType !== criteria.deviceType
                  ) {
                    continue;
                  }

                  records.push(pageView);
                }

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

  const persist: PageViewRepository["persist"] = (pageView) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const dedupDocument = operations.doc(
          firestore,
          ...toDedupPath(pageView.identifier),
        );
        const counterDocument = operations.doc(
          firestore,
          ...toCounterPath(pageView.identifier),
        );
        const accessLogCollection = accessLogCollectionOf(
          pageView.identifier.reference.type,
          pageView.identifier.reference.content,
        );
        const accessLogDocument = operations.doc(
          accessLogCollection,
          toCompositeDocumentId(pageView.identifier),
        );

        const [dedupSnapshot, counterSnapshot] = await Promise.all([
          transaction.get(dedupDocument),
          transaction.get(counterDocument),
          transaction.get(accessLogDocument),
        ]);

        if (dedupSnapshot.exists()) {
          throw duplicationError("PageView", "PageView already exists.");
        }

        const currentCount = extractCountFromSnapshot(counterSnapshot.data());

        transaction.set(dedupDocument, {
          createdAt: operations.createTimestamp(pageView.createdAt),
        });

        transaction.set(
          counterDocument,
          {
            count: currentCount + 1,
            updatedAt: operations.createTimestamp(pageView.createdAt),
          },
          { merge: true },
        );

        transaction.set(accessLogDocument, pageView);
      }),
      mapError,
    ).tap(async () => {
      await registerContentId(
        pageView.identifier.reference.type,
        pageView.identifier.reference.content,
      );
    });
  };

  const terminate: PageViewRepository["terminate"] = (identifier) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const accessLogCollection = accessLogCollectionOf(
          identifier.reference.type,
          identifier.reference.content,
        );
        const accessLogDocument = operations.doc(
          accessLogCollection,
          toCompositeDocumentId(identifier),
        );
        const counterDocument = operations.doc(
          firestore,
          ...toCounterPath(identifier),
        );
        const dedupDocument = operations.doc(
          firestore,
          ...toDedupPath(identifier),
        );

        const [accessLogSnapshot, counterSnapshot] = await Promise.all([
          transaction.get(accessLogDocument),
          transaction.get(counterDocument),
          transaction.get(dedupDocument),
        ]);

        if (!accessLogSnapshot.exists()) {
          throw aggregateNotFoundError("PageView", "PageView not found.");
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
        transaction.delete(accessLogDocument);
      }),
      mapError,
    );
  };

  return { find, search, persist, terminate };
};
