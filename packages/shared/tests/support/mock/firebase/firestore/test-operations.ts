
import type {
  DocumentData,
  FirestoreOperations,
  FirestoreTimestamp,
  Query,
  QuerySnapshot
} from "../types/firestore"
import { _clearDefaultInstance, getFirestore } from "./database/firestore-impl"
import {
  addDoc as mockAddDoc,
  deleteDoc as mockDeleteDoc,
  getDoc as mockGetDoc,
  setDoc as mockSetDoc,
  updateDoc as mockUpdateDoc
} from "./database/operations"
import {
  and as mockAnd,
  endAt as mockEndAt,
  endBefore as mockEndBefore,
  limit as mockLimit,
  limitToLast as mockLimitToLast,
  or as mockOr,
  orderBy as mockOrderBy,
  startAfter as mockStartAfter,
  startAt as mockStartAt,
  where as mockWhere
} from "./query/constraints"
import {
  collectionGroup as mockCollectionGroup,
  query as mockQuery,
  QueryImpl
} from "./query/query-impl"
import {
  CollectionReferenceImpl,
  collection as mockCollection,
  doc as mockDoc
} from "./reference/reference-impl"
import {
  runTransaction as mockRunTransaction,
  writeBatch as mockWriteBatch
} from "./transaction/transaction-functions"

async function getDocsTypeSafe<T = DocumentData>(
  query: Query<T>
): Promise<QuerySnapshot<T>> {
  if (query instanceof CollectionReferenceImpl) {
    const { getDocs } = await import("./database/operations")
    return getDocs(query) as Promise<QuerySnapshot<T>>
  } else if (
    "execute" in query &&
    typeof (query as { execute?: (...args: unknown[]) => unknown }).execute === "function"
  ) {
    const queryImpl = query as QueryImpl<T>
    return queryImpl.execute() as Promise<QuerySnapshot<T>>
  } else {
    throw new Error("Unsupported query type for getDocs operation")
  }
}

export const defaultTestFirestoreOperations: FirestoreOperations = {
  doc: mockDoc,

  collection: mockCollection,

  getDoc: mockGetDoc,
  getDocs: getDocsTypeSafe,
  setDoc: mockSetDoc,
  updateDoc: mockUpdateDoc,
  deleteDoc: mockDeleteDoc,
  addDoc: mockAddDoc,

  query: mockQuery,
  where: mockWhere,
  orderBy: mockOrderBy,
  limit: mockLimit,
  limitToLast: mockLimitToLast,
  startAt: mockStartAt,
  startAfter: mockStartAfter,
  endAt: mockEndAt,
  endBefore: mockEndBefore,
  and: mockAnd,
  or: mockOr,
  collectionGroup: mockCollectionGroup,

  runTransaction: mockRunTransaction,
  writeBatch: mockWriteBatch,

  createTimestamp: (date: Date): FirestoreTimestamp => ({
    toDate: () => date,
    seconds: Math.floor(date.getTime() / 1000),
    nanoseconds: (date.getTime() % 1000) * 1000000
  })
}

export function createTestFirestoreOperations() {
  _clearDefaultInstance()
  const firestore = getFirestore()

  return {
    firestore,
    operations: defaultTestFirestoreOperations
  }
}
