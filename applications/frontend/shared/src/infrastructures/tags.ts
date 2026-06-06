import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
} from "firebase/firestore";
import {
  createVersion,
  createTagNameIndexInTransaction,
  deleteTagNameIndexInTransaction,
  FIRESTORE_IN_BATCH_LIMIT,
  FirestoreOperations,
  getTagNameIndexPath,
  mapFirestoreError,
  TagNameIndex,
  updateTagNameIndexInTransaction,
  Version,
} from "./common";
import {
  Tag,
  TagIdentifier,
  TagName,
  TagRepository,
  Criteria,
  validateTag,
} from "@shared/domains/attributes/tag";
import { fromPromise, AsyncResult } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  UnexpectedError,
  DuplicationError,
} from "@shared/aspects/error";
import { chunk } from "@shared/aspects/array";

type PersistedTag = {
  identifier: string;
  name: string;
  logo: string;
  timeline: {
    createdAt: string;
    updatedAt: string;
  };
  version: number;
};

export const FirebaseTagRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): TagRepository => {
  const mapError = mapFirestoreError("Tag");
  const versions = new Map<TagIdentifier, Version>();

  const collection = operations
    .collection(firestore, "tags")
    .withConverter<Tag, PersistedTag>({
      toFirestore: (tag: Tag): PersistedTag => {
        const currentVersion = versions.get(tag.identifier);
        const nextVersion = currentVersion
          ? currentVersion.increment()
          : createVersion(1);

        versions.set(tag.identifier, nextVersion);

        return {
          identifier: tag.identifier,
          name: tag.name,
          logo: tag.logo,
          timeline: {
            createdAt: tag.timeline.createdAt.toISOString(),
            updatedAt: tag.timeline.updatedAt.toISOString(),
          },
          version: nextVersion.value,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedTag>,
        options?: SnapshotOptions,
      ): Tag => {
        const data = snapshot.data(options);

        const result = validateTag({
          identifier: data.identifier,
          name: data.name,
          logo: data.logo,
          timeline: {
            createdAt: new Date(data.timeline.createdAt),
            updatedAt: new Date(data.timeline.updatedAt),
          },
        });

        if (result.isErr) {
          throw new Error(
            `Failed to validate tag: ${JSON.stringify(result.unwrapError())}`,
          );
        }

        const tag = result.unwrap();

        const currentVersion = createVersion(data.version);
        versions.set(tag.identifier, currentVersion);

        return tag;
      },
    });

  return {
    find(
      identifier: TagIdentifier,
    ): AsyncResult<Tag, AggregateNotFoundError<"Tag"> | UnexpectedError> {
      return fromPromise(
        (async () => {
          const docRef = operations.doc(collection, identifier);
          const snapshot = await operations.getDoc(docRef);

          if (!snapshot.exists()) {
            throw aggregateNotFoundError("Tag", identifier);
          }

          return snapshot.data() as Tag;
        })(),
        (error) => mapError(error),
      );
    },

    search(criteria: Criteria): AsyncResult<Tag[], UnexpectedError> {
      return fromPromise(
        (async () => {
          const constraints = [];

          if (criteria.name) {
            constraints.push(operations.where("name", ">=", criteria.name));
            constraints.push(
              operations.where("name", "<=", criteria.name + "\uf8ff"),
            );
          }

          constraints.push(operations.orderBy("timeline.createdAt", "desc"));

          const queryRef = operations.query(collection, ...constraints);

          const snapshot = await operations.getDocs(queryRef);

          return snapshot.docs.map((document) => document.data() as Tag);
        })(),
        (error) => mapError(error) as UnexpectedError,
      );
    },

    ofIdentifiers(
      identifiers: TagIdentifier[],
    ): AsyncResult<Tag[], AggregateNotFoundError<"Tag"> | UnexpectedError> {
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
              return snapshot.docs.map((document) => document.data() as Tag);
            }),
          );

          const tags = new Map<TagIdentifier, Tag>();
          for (const tag of batchResults.flat()) {
            tags.set(tag.identifier, tag);
          }

          return identifiers.flatMap((identifier) => {
            const tag = tags.get(identifier);
            return tag !== undefined ? [tag] : [];
          });
        })(),
        (error) => mapError(error),
      );
    },

    ofNames(names: TagName[]): AsyncResult<Tag[], UnexpectedError> {
      return fromPromise(
        (async () => {
          const tags: Tag[] = [];

          for (const name of names) {
            const indexPath = getTagNameIndexPath(name);
            const indexRef = operations.doc(firestore, indexPath);
            const indexSnapshot = await operations.getDoc(indexRef);

            if (!indexSnapshot.exists()) {
              continue;
            }

            const indexData = indexSnapshot.data() as TagNameIndex;
            const tagIdentifier =
              indexData.referenceIdentifier as TagIdentifier;

            const docRef = operations.doc(collection, tagIdentifier);
            const snapshot = await operations.getDoc(docRef);

            if (!snapshot.exists()) {
              continue;
            }

            tags.push(snapshot.data());
          }

          return tags;
        })(),
        (error) => mapError(error) as UnexpectedError,
      );
    },

    persist(
      tag: Tag,
    ): AsyncResult<void, UnexpectedError | DuplicationError<"Tag">> {
      return fromPromise(
        operations.runTransaction(firestore, async (transaction) => {
          const rawDocRef = operations.doc(firestore, "tags", tag.identifier);
          const snapshot = await transaction.get(rawDocRef);
          const currentVersion = versions.get(tag.identifier);

          if (currentVersion && currentVersion.value > 0) {
            if (!snapshot.exists()) {
              throw aggregateNotFoundError(
                "Tag",
                `Tag ${tag.identifier} not found for update.`,
              );
            }

            const existingData = snapshot.data() as PersistedTag;
            const oldName = existingData.name;

            await updateTagNameIndexInTransaction(
              transaction,
              operations,
              firestore,
              {
                oldName,
                newName: tag.name,
                referenceIdentifier: tag.identifier,
              },
            );

            const nextVersion = currentVersion.increment();
            const persistedTag: PersistedTag = {
              identifier: tag.identifier,
              name: tag.name,
              logo: tag.logo,
              timeline: {
                createdAt: tag.timeline.createdAt.toISOString(),
                updatedAt: tag.timeline.updatedAt.toISOString(),
              },
              version: nextVersion.value,
            };
            transaction.set(rawDocRef, persistedTag);
            versions.set(tag.identifier, nextVersion);
          } else {
            await createTagNameIndexInTransaction(
              transaction,
              operations,
              firestore,
              {
                name: tag.name,
                referenceIdentifier: tag.identifier,
              },
            );

            const persistedTag: PersistedTag = {
              identifier: tag.identifier,
              name: tag.name,
              logo: tag.logo,
              timeline: {
                createdAt: tag.timeline.createdAt.toISOString(),
                updatedAt: tag.timeline.updatedAt.toISOString(),
              },
              version: 1,
            };
            transaction.set(rawDocRef, persistedTag);
            versions.set(tag.identifier, createVersion(1));
          }
        }),
        (error) => mapError(error),
      );
    },

    terminate(
      identifier: TagIdentifier,
    ): AsyncResult<void, AggregateNotFoundError<"Tag"> | UnexpectedError> {
      return fromPromise(
        operations.runTransaction(firestore, async (transaction) => {
          const rawDocRef = operations.doc(firestore, "tags", identifier);
          const snapshot = await transaction.get(rawDocRef);

          if (!snapshot.exists()) {
            throw aggregateNotFoundError("Tag", identifier);
          }

          const tagData = snapshot.data() as PersistedTag;

          deleteTagNameIndexInTransaction(transaction, operations, firestore, {
            name: tagData.name,
          });

          transaction.delete(rawDocRef);
          versions.delete(identifier);
        }),
        (error) => mapError(error),
      );
    },
  };
};
