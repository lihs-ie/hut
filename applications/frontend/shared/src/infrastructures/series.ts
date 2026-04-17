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
import { Order, SortByField } from "@shared/domains/common/sort";
import { fromPromise } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  duplicationError,
} from "@shared/aspects/error";
import { chunk, FIRESTORE_IN_BATCH_LIMIT } from "@shared/aspects/array";

type PersistedSeries = {
  identifier: string;
  title: string;
  slug: string;
  tags: string[];
  description?: string;
  subTitle?: string | null;
  cover: string | null;
  status: string;
  chapters: string[];
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
          status: series.status,
          chapters: series.chapters,
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
          status: data.status ?? "published",
          chapters: data.chapters ?? [],
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
        const constraints = [];

        if (criteria.slug) {
          constraints.push(operations.where("slug", "==", criteria.slug));
        }

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

        const seriesList: Series[] = [];
        querySnapshot.forEach((doc) => {
          seriesList.push(doc.data());
        });

        if (criteria.freeWord) {
          const keyword = criteria.freeWord.toLowerCase();
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
        if (identifiers.length === 0) {
          return [];
        }

        const chunks = chunk(identifiers, FIRESTORE_IN_BATCH_LIMIT);

        const batchResults = await Promise.all(
          chunks.map(async (idsChunk) => {
            const q = operations.query(
              collection,
              operations.where("identifier", "in", idsChunk),
            );
            const snapshot = await operations.getDocs(q);
            return snapshot.docs.map((document) => document.data());
          }),
        );

        const seriesList = batchResults.flat();

        if (throwOnMissing && seriesList.length !== identifiers.length) {
          const foundIds = new Set(seriesList.map((series) => series.identifier));
          const missingIdentifiers = identifiers.filter(
            (identifier) => !foundIds.has(identifier),
          );
          const firstMissing = missingIdentifiers[0];
          throw aggregateNotFoundError(
            "Series",
            `Series ${firstMissing} not found.`,
          );
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
