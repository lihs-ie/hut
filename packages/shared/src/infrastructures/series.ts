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
  Series,
  SeriesIdentifier,
  SeriesRepository,
  validateSeries,
} from "@/domains/series";
import { fromPromise } from "@/aspects/result";
import { aggregateNotFoundError, duplicationError } from "@/aspects/error";

type PersistedSeries = {
  identifier: string;
  title: string;
  description?: string;
  cover: string | null;
  pages: Array<{
    title: string;
    content: string;
    timeline: {
      createdAt: Date;
      updatedAt: Date;
    };
  }>;
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
  version: number;
};

export const FirebaseSeriesRepository = (
  firestore: Firestore,
  operations: FirestoreOperations
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
          pages: series.pages.map((page) => ({
            title: page.title,
            content: page.content,
            timeline: {
              createdAt: page.timeline.createdAt,
              updatedAt: page.timeline.updatedAt,
            },
          })),
          timeline: {
            createdAt: series.timeline.createdAt,
            updatedAt: series.timeline.updatedAt,
          },
          version: currentVersion?.increment().value ?? 1,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedSeries>,
        options?: SnapshotOptions
      ): Series => {
        const data = snapshot.data(options);

        const series = validateSeries({
          identifier: data.identifier,
          title: data.title,
          description: data.description,
          cover: data.cover,
          pages: data.pages,
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
              `Series ${series.identifier} not found for update.`
            );
          }

          transaction.set(document, series, { merge: true });
        } else {
          if (snapshot.exists()) {
            throw duplicationError("Series", series.identifier);
          }

          transaction.set(document, series);
        }
      }),
      mapError
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
            `Series ${identifier} not found.`
          );
        }

        return snapshot.data();
      })(),
      mapError
    );
  };

  const search: SeriesRepository["search"] = (title: string) => {
    return fromPromise(
      (async () => {
        const q = operations.query(collection);
        const querySnapshot = await operations.getDocs(q);

        const seriesList: Series[] = [];
        querySnapshot.forEach((doc) => {
          seriesList.push(doc.data());
        });

        if (title) {
          const keyword = title.toLowerCase();
          return seriesList.filter((series) =>
            series.title.toLowerCase().includes(keyword)
          );
        }

        return seriesList;
      })(),
      mapError
    );
  };

  const terminate: SeriesRepository["terminate"] = (
    identifier: SeriesIdentifier
  ) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, identifier);
        const snapshot = await transaction.get(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "Series",
            `Series ${identifier} not found for deletion.`
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
