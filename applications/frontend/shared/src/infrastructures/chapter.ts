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
  Chapter,
  ChapterIdentifier,
  ChapterRepository,
  validateChapter,
} from "@shared/domains/series/chapter";
import { Slug } from "@shared/domains/common";
import { fromPromise } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  duplicationError,
} from "@shared/aspects/error";
import { chunk, FIRESTORE_IN_BATCH_LIMIT } from "@shared/aspects/array";

type PersistedChapter = {
  identifier: string;
  title: string;
  slug: string;
  content: string;
  images: string[];
  status: string;
  publishedAt: Timestamp | null;
  timeline: {
    createdAt: Timestamp;
    updatedAt: Timestamp;
  };
  version: number;
};

export const FirebaseChapterRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): ChapterRepository => {
  const mapError = mapFirestoreError("Chapter");
  const versions: Map<ChapterIdentifier, Version> = new Map();

  const collection = operations
    .collection(firestore, "chapters")
    .withConverter<Chapter, PersistedChapter>({
      toFirestore: (chapter: Chapter): PersistedChapter => {
        const currentVersion = versions.get(chapter.identifier);

        return {
          identifier: chapter.identifier,
          title: chapter.title,
          slug: chapter.slug,
          content: chapter.content,
          images: chapter.images,
          status: chapter.status,
          publishedAt: chapter.publishedAt
            ? Timestamp.fromDate(chapter.publishedAt)
            : null,
          timeline: {
            createdAt: Timestamp.fromDate(chapter.timeline.createdAt),
            updatedAt: Timestamp.fromDate(chapter.timeline.updatedAt),
          },
          version: currentVersion?.increment().value ?? 1,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedChapter>,
        options?: SnapshotOptions,
      ): Chapter => {
        const data = snapshot.data(options);

        const chapter = validateChapter({
          identifier: data.identifier,
          title: data.title,
          slug: data.slug,
          content: data.content,
          images: data.images ?? [],
          status: data.status ?? "published",
          publishedAt: data.publishedAt ? data.publishedAt.toDate() : null,
          timeline: {
            createdAt: data.timeline.createdAt.toDate(),
            updatedAt: data.timeline.updatedAt.toDate(),
          },
        }).unwrap();

        const version = createVersion(data.version);
        versions.set(chapter.identifier, version);

        return chapter;
      },
    });

  const find: ChapterRepository["find"] = (identifier: ChapterIdentifier) => {
    return fromPromise(
      (async () => {
        const document = operations.doc(collection, identifier);
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "Chapter",
            `Chapter ${identifier} not found.`,
          );
        }

        return snapshot.data();
      })(),
      mapError,
    );
  };

  const findBySlug: ChapterRepository["findBySlug"] = (slug: Slug) => {
    return fromPromise(
      (async () => {
        const query = operations.query(
          collection,
          operations.where("slug", "==", slug),
        );
        const querySnapshot = await operations.getDocs(query);

        if (querySnapshot.empty) {
          throw aggregateNotFoundError(
            "Chapter",
            `Chapter with slug ${slug} not found.`,
          );
        }

        const document = querySnapshot.docs[0];
        return document.data();
      })(),
      mapError,
    );
  };

  const persist: ChapterRepository["persist"] = (chapter: Chapter) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, chapter.identifier);
        const snapshot = await transaction.get(document);
        const currentVersion = versions.get(chapter.identifier);

        if (currentVersion && currentVersion.value > 0) {
          if (!snapshot.exists()) {
            throw aggregateNotFoundError(
              "Chapter",
              `Chapter ${chapter.identifier} not found for update.`,
            );
          }

          const existingData = snapshot.data();
          const oldSlug = existingData.slug;

          await updateSlugIndexInTransaction(
            transaction,
            operations,
            firestore,
            {
              collectionName: "chapters",
              oldSlug,
              newSlug: chapter.slug,
              referenceIdentifier: chapter.identifier,
              aggregateName: "Chapter",
            },
          );

          transaction.set(document, chapter, { merge: true });
          versions.set(chapter.identifier, currentVersion.increment());
        } else {
          if (snapshot.exists()) {
            throw duplicationError("Chapter", chapter.identifier);
          }

          await createSlugIndexInTransaction(
            transaction,
            operations,
            firestore,
            {
              collectionName: "chapters",
              slug: chapter.slug,
              referenceIdentifier: chapter.identifier,
              aggregateName: "Chapter",
            },
          );

          transaction.set(document, chapter);
          versions.set(chapter.identifier, createVersion(1));
        }
      }),
      mapError,
    );
  };

  const terminate: ChapterRepository["terminate"] = (
    identifier: ChapterIdentifier,
  ) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, identifier);
        const snapshot = await transaction.get(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "Chapter",
            `Chapter ${identifier} not found for deletion.`,
          );
        }

        const chapter = snapshot.data();
        deleteSlugIndexInTransaction(transaction, operations, firestore, {
          collectionName: "chapters",
          slug: chapter.slug,
        });

        transaction.delete(document);
        versions.delete(identifier);
      }),
      mapError,
    );
  };

  const ofIdentifiers: ChapterRepository["ofIdentifiers"] = (
    identifiers: ChapterIdentifier[],
    throwOnMissing = false,
  ) => {
    return fromPromise(
      (async () => {
        if (identifiers.length === 0) {
          return [];
        }

        const uniqueIdentifiers = Array.from(new Set(identifiers));
        const chunks = chunk(uniqueIdentifiers, FIRESTORE_IN_BATCH_LIMIT);

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

        const chapterByIdentifier = new Map<ChapterIdentifier, Chapter>();
        for (const chapter of batchResults.flat()) {
          chapterByIdentifier.set(chapter.identifier, chapter);
        }

        if (throwOnMissing) {
          const missingIdentifier = uniqueIdentifiers.find(
            (identifier) => !chapterByIdentifier.has(identifier),
          );
          if (missingIdentifier !== undefined) {
            throw aggregateNotFoundError(
              "Chapter",
              `Chapter ${missingIdentifier} not found.`,
            );
          }
        }

        const chapterList: Chapter[] = [];
        for (const identifier of identifiers) {
          const chapter = chapterByIdentifier.get(identifier);
          if (chapter !== undefined) {
            chapterList.push(chapter);
          }
        }
        return chapterList;
      })(),
      mapError,
    );
  };

  return {
    find,
    findBySlug,
    ofIdentifiers,
    persist,
    terminate,
  };
};
