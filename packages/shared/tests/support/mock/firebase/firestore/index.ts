
export {
  getFirestore,
  initializeFirestore,
  _clearDefaultInstance
} from "./database/firestore-impl"

export { collection, doc } from "./reference/reference-impl"

export {
  addDoc,
  setDoc,
  getDoc,
  updateDoc,
  deleteDoc,
  getDocs
} from "./database/operations"

export { query, collectionGroup } from "./query/query-impl"
export {
  where,
  orderBy,
  limit,
  limitToLast,
  startAt,
  startAfter,
  endAt,
  endBefore,
  and,
  or
} from "./query/constraints"

export { runTransaction, writeBatch } from "./transaction/transaction-functions"

export {
  defaultTestFirestoreOperations as mockFirestoreOperations,
  createTestFirestoreOperations
} from "./test-operations"

export {
  seedFirestore,
  createTestFirestoreWithSeed,
  seedCollection,
  loadFixture,
  clearFirestore,
  addDocsWithAutoId,
  addDocWithAutoId,
  seedFirestoreWithAutoIds
} from "./test-seed"
export type { FirestoreFixture } from "./test-seed"

export type {
  Firestore,
  DocumentReference,
  CollectionReference,
  DocumentSnapshot,
  QueryDocumentSnapshot,
  QuerySnapshot,
  DocumentData,
  FirestoreSettings,
  SetOptions,
  UpdateData,
  Transaction,
  WriteBatch,
  TransactionFunction
} from "../types/firestore"
