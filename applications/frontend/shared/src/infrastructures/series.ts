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
  FirestoreOperations,
  mapFirestoreError,
  updateSlugIndexInTransaction,
  Version,
} from "./common";
import {
  Criteria,
  Series,
  SeriesIdentifier,
  SeriesRepository,
  SeriesSlug,
  validateSeries,
} from "@shared/domains/series";
import { fromPromise } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  duplicationError,
} from "@shared/aspects/error";

type PersistedSeries = {
  identifier: string;
  title: string;
  slug: string;
  tags: string[];
  description?: string;
  subTitle?: string | null;
  cover: string | null;
  chapters: Array<{
    title: string;
    content: string;
    slug: string;
    timeline: {
      createdAt: Timestamp;
      updatedAt: Timestamp;
    };
  }>;
  timeline: {
    createdAt: Timestamp;
    updatedAt: Timestamp;
  };
  version: number;
};

export const FirebaseSeriesRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): SeriesRepository => {
  const mapError = mapFirestoreError("Series");
  const versions: Map<SeriesIdentifier, Version> = new Map();

  const collection = operations
    .collection(firestore, "series")
    .withConverter<Series, PersistedSeries>({
      toFirestore: (series: Series): PersistedSeries => {
        const currentVersion = versions.get(series.identifier);

        return {
          identifier: series.identifier,
          title: series.title,
          description: series.description,
          cover: series.cover,
          slug: series.slug,
          tags: series.tags,
          subTitle: series.subTitle,
          chapters: series.chapters.map((chapter) => ({
            title: chapter.title,
            slug: chapter.slug,
            content: chapter.content,
            timeline: {
              createdAt: Timestamp.fromDate(chapter.timeline.createdAt),
              updatedAt: Timestamp.fromDate(chapter.timeline.updatedAt),
            },
          })),
          timeline: {
            createdAt: Timestamp.fromDate(series.timeline.createdAt),
            updatedAt: Timestamp.fromDate(series.timeline.updatedAt),
          },
          version: currentVersion?.increment().value ?? 1,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedSeries>,
        options?: SnapshotOptions,
      ): Series => {
        const data = snapshot.data(options);

        const series = validateSeries({
          identifier: data.identifier,
          title: data.title,
          description: data.description,
          cover: data.cover,
          slug: data.slug,
          tags: data.tags,
          subTitle: data.subTitle || null,
          chapters: data.chapters.map((chapter) => ({
            title: chapter.title,
            slug: chapter.slug,
            content: chapter.content,
            timeline: {
              createdAt: chapter.timeline.createdAt.toDate(),
              updatedAt: chapter.timeline.updatedAt.toDate(),
            },
          })),
          timeline: {
            createdAt: data.timeline.createdAt.toDate(),
            updatedAt: data.timeline.updatedAt.toDate(),
          },
        }).unwrap();

        const version = createVersion(data.version);

        versions.set(series.identifier, version);

        return series;
      },
    });

  const persist: SeriesRepository["persist"] = (series: Series) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, series.identifier);
        const snapshot = await transaction.get(document);
        const currentVersion = versions.get(series.identifier);

        if (currentVersion && currentVersion.value > 0) {
          if (!snapshot.exists()) {
            throw aggregateNotFoundError(
              "Series",
              `Series ${series.identifier} not found for update.`,
            );
          }

          const existingData = snapshot.data();
          const oldSlug = existingData.slug;

          await updateSlugIndexInTransaction(
            transaction,
            operations,
            firestore,
            {
              collectionName: "series",
              oldSlug,
              newSlug: series.slug,
              referenceIdentifier: series.identifier,
              aggregateName: "Series",
            },
          );

          transaction.set(document, series, { merge: true });
          versions.set(series.identifier, currentVersion.increment());
        } else {
          if (snapshot.exists()) {
            throw duplicationError("Series", series.identifier);
          }

          await createSlugIndexInTransaction(
            transaction,
            operations,
            firestore,
            {
              collectionName: "series",
              slug: series.slug,
              referenceIdentifier: series.identifier,
              aggregateName: "Series",
            },
          );

          transaction.set(document, series);
          versions.set(series.identifier, createVersion(1));
        }
      }),
      mapError,
    );
  };

  const find: SeriesRepository["find"] = (identifier: SeriesIdentifier) => {
    return fromPromise(
      (async () => {
        const document = operations.doc(collection, identifier);
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "Series",
            `Series ${identifier} not found.`,
          );
        }

        return snapshot.data();
      })(),
      mapError,
    );
  };

  const findBySlug: SeriesRepository["findBySlug"] = (slug: SeriesSlug) => {
    return fromPromise(
      (async () => {
        const query = operations.query(
          collection,
          operations.where("slug", "==", slug),
        );
        const querySnapshot = await operations.getDocs(query);

        if (querySnapshot.empty) {
          throw aggregateNotFoundError(
            "Series",
            `Series with slug ${slug} not found.`,
          );
        }

        const document = querySnapshot.docs[0];
        return document.data();
      })(),
      mapError,
    );
  };

  const search: SeriesRepository["search"] = (criteria: Criteria) => {
    return fromPromise(
      (async () => {
        const q = operations.query(collection);
        const querySnapshot = await operations.getDocs(q);

        const seriesList: Series[] = [];
        querySnapshot.forEach((doc) => {
          seriesList.push(doc.data());
        });

        if (criteria.slug) {
          const keyword = criteria.slug.toLowerCase();
          return seriesList.filter((series) =>
            series.title.toLowerCase().includes(keyword),
          );
        }

        return seriesList;
      })(),
      mapError,
    );
  };

  const terminate: SeriesRepository["terminate"] = (
    identifier: SeriesIdentifier,
  ) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, identifier);
        const snapshot = await transaction.get(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "Series",
            `Series ${identifier} not found for deletion.`,
          );
        }

        const series = snapshot.data();
        deleteSlugIndexInTransaction(transaction, operations, firestore, {
          collectionName: "series",
          slug: series.slug,
        });

        transaction.delete(document);
        versions.delete(identifier);
      }),
      mapError,
    );
  };

  const ofIdentifiers: SeriesRepository["ofIdentifiers"] = (
    identifiers: SeriesIdentifier[],
    throwOnMissing = false,
  ) => {
    return fromPromise(
      (async () => {
        const seriesList: Series[] = [];

        for (const identifier of identifiers) {
          const document = operations.doc(collection, identifier);
          const snapshot = await operations.getDoc(document);

          if (!snapshot.exists()) {
            if (throwOnMissing) {
              throw aggregateNotFoundError(
                "Series",
                `Series ${identifier} not found.`,
              );
            }
            continue;
          }

          seriesList.push(snapshot.data());
        }

        return seriesList;
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
