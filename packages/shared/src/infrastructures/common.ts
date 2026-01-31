import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  DuplicationError,
  duplicationError,
  unexpectedError,
  UnexpectedError,
} from "@shared/aspects/error";

export type SlugIndex = {
  referenceIdentifier: string;
  collectionName: string;
  createdAt: Date;
};

export type SlugIndexCollectionName = "articles" | "memos" | "series";

export const getSlugIndexPath = (
  collectionName: SlugIndexCollectionName,
  slug: string,
): string => `index/${collectionName}/slug/${slug}`;

export type TagNameIndex = {
  referenceIdentifier: string;
  createdAt: Date;
};

export const getTagNameIndexPath = (name: string): string =>
  `index/tags/name/${name}`;
import { AsyncResult, fromPromise } from "@shared/aspects/result";
import { ImageUploader } from "@shared/domains/common/image";
import type {
  CollectionReference,
  DocumentData,
  DocumentReference,
  DocumentSnapshot,
  Timestamp as FirestoreTimestamp,
  OrderByDirection,
  Query,
  QueryCompositeFilterConstraint,
  QueryConstraint,
  QueryFilterConstraint,
  QuerySnapshot,
  SetOptions,
  Transaction,
  UpdateData,
  WhereFilterOp,
  WriteBatch,
} from "firebase/firestore";
import { type Firestore } from "firebase/firestore";
import {
  FirebaseStorage,
  StorageError,
  StorageErrorCode,
} from "firebase/storage";
import { getDownloadURL, ref, uploadBytes } from "firebase/storage";

type TransactionFunction<T> = (transaction: Transaction) => Promise<T>;

export interface FirestoreOperations {
  doc<T = DocumentData>(
    firestore: Firestore,
    path: string,
    ...pathSegments: string[]
  ): DocumentReference<T>;
  doc<T = DocumentData>(
    reference: CollectionReference<T>,
    path?: string,
    ...pathSegments: string[]
  ): DocumentReference<T>;
  doc<T = DocumentData>(
    reference: DocumentReference<T>,
    path: string,
    ...pathSegments: string[]
  ): DocumentReference<T>;

  collection<T = DocumentData>(
    firestore: Firestore,
    path: string,
    ...pathSegments: string[]
  ): CollectionReference<T>;

  getDoc<T = DocumentData>(
    reference: DocumentReference<T>,
  ): Promise<DocumentSnapshot<T>>;
  getDocs<T = DocumentData>(query: Query<T>): Promise<QuerySnapshot<T>>;
  setDoc<T = DocumentData>(
    reference: DocumentReference<T>,
    data: T,
    options?: SetOptions,
  ): Promise<void>;
  updateDoc<T = DocumentData>(
    reference: DocumentReference<T>,
    data: UpdateData<T>,
  ): Promise<void>;
  deleteDoc(reference: DocumentReference<DocumentData>): Promise<void>;
  addDoc<T = DocumentData>(
    reference: CollectionReference<T>,
    data: T,
  ): Promise<DocumentReference<T>>;

  query<T = DocumentData>(
    query: Query<T>,
    ...constraints: QueryConstraint[]
  ): Query<T>;
  where(
    fieldPath: string,
    opStr: WhereFilterOp,
    value: unknown,
  ): QueryConstraint;
  orderBy(fieldPath: string, directionStr?: OrderByDirection): QueryConstraint;
  limit(limit: number): QueryConstraint;
  limitToLast(limit: number): QueryConstraint;
  startAt(...values: unknown[]): QueryConstraint;
  startAfter(...values: unknown[]): QueryConstraint;
  endAt(...values: unknown[]): QueryConstraint;
  endBefore(...values: unknown[]): QueryConstraint;
  and(
    ...queryConstraints: QueryFilterConstraint[]
  ): QueryCompositeFilterConstraint;
  or(
    ...queryConstraints: QueryFilterConstraint[]
  ): QueryCompositeFilterConstraint;
  collectionGroup(
    firestore: Firestore,
    collectionId: string,
  ): Query<DocumentData>;

  runTransaction<T>(
    firestore: Firestore,
    updateFunction: TransactionFunction<T>,
  ): Promise<T>;
  writeBatch(firestore: Firestore): WriteBatch;

  createTimestamp(date: Date): FirestoreTimestamp;
}

type FirebaseLikeError = { code?: string; message?: string } & Record<
  string,
  unknown
>;

const isDomainError = (
  value: unknown,
): value is
  | AggregateNotFoundError<string>
  | DuplicationError<string>
  | UnexpectedError =>
  value !== null &&
  typeof value === "object" &&
  "_tag" in value &&
  typeof (value as { _tag: unknown })._tag === "symbol";

export const mapFirestoreError = <N extends string>(aggregateName: N) => {
  return (
    error: unknown,
  ): AggregateNotFoundError<N> | DuplicationError<N> | UnexpectedError => {
    if (isDomainError(error)) {
      return error as
        | AggregateNotFoundError<N>
        | DuplicationError<N>
        | UnexpectedError;
    }

    const code = (error as FirebaseLikeError)?.code ?? "unknown";
    const message = (error as FirebaseLikeError)?.message ?? String(error);

    switch (code) {
      case "not-found":
        return aggregateNotFoundError(aggregateName, message);
      case "already-exists":
        return duplicationError(aggregateName, message);

      case "unauthenticated":
      case "permission-denied":
      case "resource-exhausted":
      case "unavailable":
      case "deadline-exceeded":
      case "aborted":
      case "failed-precondition":
      case "invalid-argument":
      case "out-of-range":
      case "internal":
      case "data-loss":
      case "cancelled":
      case "unimplemented":
      case "unknown":
      default:
        return unexpectedError(message, error);
    }
  };
};

export interface Version {
  value: number;
  increment(): Version;
}

export const createVersion = (value: number): Version => {
  return {
    value,
    increment(): Version {
      return createVersion(this.value + 1);
    },
  };
};

export const mapFirebaseStorageError = (error: unknown): UnexpectedError => {
  if (error instanceof StorageError) {
    switch (error.code) {
      case StorageErrorCode.UNAUTHORIZED:
        return unexpectedError(
          "Unauthorized access to the storage resource",
          error,
        );
      case StorageErrorCode.CANCELED:
        return unexpectedError("Upload has been canceled", error);
      case StorageErrorCode.QUOTA_EXCEEDED:
        return unexpectedError("Storage quota has been exceeded", error);
      case StorageErrorCode.RETRY_LIMIT_EXCEEDED:
        return unexpectedError("Upload retry limit has been exceeded", error);
      case StorageErrorCode.INVALID_CHECKSUM:
        return unexpectedError("File checksum does not match", error);
      case StorageErrorCode.OBJECT_NOT_FOUND:
        return unexpectedError("The specified file was not found", error);
      case StorageErrorCode.BUCKET_NOT_FOUND:
        return unexpectedError("Storage bucket not found", error);
      case StorageErrorCode.INVALID_URL:
        return unexpectedError("Invalid URL format", error);
      default:
        return unexpectedError(error.message, error);
    }
  }

  const message = error instanceof Error ? error.message : String(error);
  return unexpectedError(message, error);
};

export const FirebaseStorageImageUploader = (
  storage: FirebaseStorage,
): ImageUploader => {
  const upload: ImageUploader["upload"] = <T, R>(
    image: T,
    path: string,
  ): AsyncResult<R, UnexpectedError> =>
    fromPromise(
      (async () => {
        const storageRef = ref(storage, path);

        const snapshot = await uploadBytes(storageRef, image as Blob);

        const url = await getDownloadURL(snapshot.ref);

        return url as R;
      })(),
      mapFirebaseStorageError,
    );

  return { upload };
};

export const createSlugIndexInTransaction = async <N extends string>(
  transaction: Transaction,
  operations: FirestoreOperations,
  firestore: Firestore,
  params: {
    collectionName: SlugIndexCollectionName;
    slug: string;
    referenceIdentifier: string;
    aggregateName: N;
  },
): Promise<void> => {
  const indexPath = getSlugIndexPath(params.collectionName, params.slug);
  const indexRef = operations.doc(firestore, indexPath);
  const indexSnapshot = await transaction.get(indexRef);

  if (indexSnapshot.exists()) {
    throw duplicationError(
      params.aggregateName,
      `Slug "${params.slug}" is already in use`,
    );
  }

  const indexData: SlugIndex = {
    referenceIdentifier: params.referenceIdentifier,
    collectionName: params.collectionName,
    createdAt: new Date(),
  };

  transaction.set(indexRef, indexData);
};

export const updateSlugIndexInTransaction = async <N extends string>(
  transaction: Transaction,
  operations: FirestoreOperations,
  firestore: Firestore,
  params: {
    collectionName: SlugIndexCollectionName;
    oldSlug: string;
    newSlug: string;
    referenceIdentifier: string;
    aggregateName: N;
  },
): Promise<void> => {
  if (params.oldSlug === params.newSlug) {
    return;
  }

  const newIndexPath = getSlugIndexPath(params.collectionName, params.newSlug);
  const newIndexRef = operations.doc(firestore, newIndexPath);
  const newIndexSnapshot = await transaction.get(newIndexRef);

  if (newIndexSnapshot.exists()) {
    throw duplicationError(
      params.aggregateName,
      `Slug "${params.newSlug}" is already in use`,
    );
  }

  const oldIndexPath = getSlugIndexPath(params.collectionName, params.oldSlug);
  const oldIndexRef = operations.doc(firestore, oldIndexPath);
  transaction.delete(oldIndexRef);

  const indexData: SlugIndex = {
    referenceIdentifier: params.referenceIdentifier,
    collectionName: params.collectionName,
    createdAt: new Date(),
  };

  transaction.set(newIndexRef, indexData);
};

export const deleteSlugIndexInTransaction = (
  transaction: Transaction,
  operations: FirestoreOperations,
  firestore: Firestore,
  params: {
    collectionName: SlugIndexCollectionName;
    slug: string;
  },
): void => {
  const indexPath = getSlugIndexPath(params.collectionName, params.slug);
  const indexRef = operations.doc(firestore, indexPath);
  transaction.delete(indexRef);
};

export const createTagNameIndexInTransaction = async (
  transaction: Transaction,
  operations: FirestoreOperations,
  firestore: Firestore,
  params: {
    name: string;
    referenceIdentifier: string;
  },
): Promise<void> => {
  const indexPath = getTagNameIndexPath(params.name);
  const indexRef = operations.doc(firestore, indexPath);
  const indexSnapshot = await transaction.get(indexRef);

  if (indexSnapshot.exists()) {
    throw duplicationError(
      "Tag",
      `Tag name "${params.name}" is already in use`,
    );
  }

  const indexData: TagNameIndex = {
    referenceIdentifier: params.referenceIdentifier,
    createdAt: new Date(),
  };

  transaction.set(indexRef, indexData);
};

export const updateTagNameIndexInTransaction = async (
  transaction: Transaction,
  operations: FirestoreOperations,
  firestore: Firestore,
  params: {
    oldName: string;
    newName: string;
    referenceIdentifier: string;
  },
): Promise<void> => {
  if (params.oldName === params.newName) {
    return;
  }

  const newIndexPath = getTagNameIndexPath(params.newName);
  const newIndexRef = operations.doc(firestore, newIndexPath);
  const newIndexSnapshot = await transaction.get(newIndexRef);

  if (newIndexSnapshot.exists()) {
    throw duplicationError(
      "Tag",
      `Tag name "${params.newName}" is already in use`,
    );
  }

  const oldIndexPath = getTagNameIndexPath(params.oldName);
  const oldIndexRef = operations.doc(firestore, oldIndexPath);
  transaction.delete(oldIndexRef);

  const indexData: TagNameIndex = {
    referenceIdentifier: params.referenceIdentifier,
    createdAt: new Date(),
  };

  transaction.set(newIndexRef, indexData);
};

export const deleteTagNameIndexInTransaction = (
  transaction: Transaction,
  operations: FirestoreOperations,
  firestore: Firestore,
  params: {
    name: string;
  },
): void => {
  const indexPath = getTagNameIndexPath(params.name);
  const indexRef = operations.doc(firestore, indexPath);
  transaction.delete(indexRef);
};
