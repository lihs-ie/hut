import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
  Timestamp,
} from "firebase/firestore";
import {
  createVersion,
  FirestoreOperations,
  mapFirestoreError,
  Version,
} from "./common";
import {
  SiteDocument,
  SiteDocumentRepository,
  validateSiteDocument,
} from "@shared/domains/document";
import { fromPromise } from "@shared/aspects/result";
import { aggregateNotFoundError } from "@shared/aspects/error";

const DOCUMENT_ID = "site-document";

type PersistedPrivacyPolicySection = {
  headline: string;
  body: string;
  list: string[] | null;
};

type PersistedTimeline = {
  createdAt: Timestamp;
  updatedAt: Timestamp;
};

type PersistedPrivacyPolicy = {
  sections: PersistedPrivacyPolicySection[];
  timeline: PersistedTimeline;
};

type PersistedSiteDocument = {
  privacy: PersistedPrivacyPolicy;
  version: number;
};

export const FirebaseSiteDocumentRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): SiteDocumentRepository => {
  const mapError = mapFirestoreError("SiteDocument");
  let currentVersion: Version | null = null;

  const collection = operations
    .collection(firestore, "site-documents")
    .withConverter<SiteDocument, PersistedSiteDocument>({
      toFirestore: (document: SiteDocument): PersistedSiteDocument => {
        return {
          privacy: {
            sections: document.privacy.sections.map((section) => ({
              headline: section.headline,
              body: section.body,
              list: section.list,
            })),
            timeline: {
              createdAt: Timestamp.fromDate(
                document.privacy.timeline.createdAt,
              ),
              updatedAt: Timestamp.fromDate(
                document.privacy.timeline.updatedAt,
              ),
            },
          },
          version: currentVersion?.increment().value ?? 1,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedSiteDocument>,
        options?: SnapshotOptions,
      ): SiteDocument => {
        const data = snapshot.data(options);

        const siteDocument = validateSiteDocument({
          privacy: {
            sections: data.privacy.sections,
            timeline: {
              createdAt: data.privacy.timeline.createdAt.toDate(),
              updatedAt: data.privacy.timeline.updatedAt.toDate(),
            },
          },
        }).unwrap();

        currentVersion = createVersion(data.version);

        return siteDocument;
      },
    });

  const find: SiteDocumentRepository["find"] = () => {
    return fromPromise(
      (async () => {
        const document = operations.doc(collection, DOCUMENT_ID);
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "SiteDocument",
            "SiteDocument not found.",
          );
        }

        return snapshot.data();
      })(),
      mapError,
    );
  };

  const persist: SiteDocumentRepository["persist"] = (
    document: SiteDocument,
  ) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const docRef = operations.doc(collection, DOCUMENT_ID);
        const snapshot = await transaction.get(docRef);

        if (currentVersion && currentVersion.value > 0) {
          if (!snapshot.exists()) {
            throw aggregateNotFoundError(
              "SiteDocument",
              "SiteDocument not found for update.",
            );
          }
          transaction.set(docRef, document, { merge: true });
          currentVersion = currentVersion.increment();
        } else {
          transaction.set(docRef, document);
          currentVersion = createVersion(1);
        }
      }),
      mapError,
    );
  };

  return {
    find,
    persist,
  };
};
