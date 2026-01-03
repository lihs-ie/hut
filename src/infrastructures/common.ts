import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  DuplicationError,
  duplicationError,
  unexpectedError,
  UnexpectedError,
} from "@/aspects/error";
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
    reference: DocumentReference<T>
  ): Promise<DocumentSnapshot<T>>;
  getDocs<T = DocumentData>(query: Query<T>): Promise<QuerySnapshot<T>>;
  setDoc<T = DocumentData>(
    reference: DocumentReference<T>,
    data: T,
    options?: SetOptions
  ): Promise<void>;
  updateDoc<T = DocumentData>(
    reference: DocumentReference<T>,
    data: UpdateData<T>
  ): Promise<void>;
  deleteDoc(reference: DocumentReference<DocumentData>): Promise<void>;
  addDoc<T = DocumentData>(
    reference: CollectionReference<T>,
    data: T
  ): Promise<DocumentReference<T>>;

  query<T = DocumentData>(
    query: Query<T>,
    ...constraints: QueryConstraint[]
  ): Query<T>;
  where(
    fieldPath: string,
    opStr: WhereFilterOp,
    value: unknown
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
    collectionId: string
  ): Query<DocumentData>;

  runTransaction<T>(
    firestore: Firestore,
    updateFunction: TransactionFunction<T>
  ): Promise<T>;
  writeBatch(firestore: Firestore): WriteBatch;

  // Utility functions
  createTimestamp(date: Date): FirestoreTimestamp;
}

type FirebaseLikeError = { code?: string; message?: string } & Record<
  string,
  unknown
>;

export const mapFirestoreError = <N extends string>(aggregateName: N) => {
  return (
    error: unknown
  ): AggregateNotFoundError<N> | DuplicationError<N> | UnexpectedError => {
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
