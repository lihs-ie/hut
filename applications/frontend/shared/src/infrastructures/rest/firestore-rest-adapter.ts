import {
  addDoc,
  and,
  collection,
  collectionGroup,
  deleteDoc,
  doc,
  endAt,
  endBefore,
  getDoc,
  getDocs,
  limit,
  limitToLast,
  or,
  orderBy,
  query,
  runTransaction,
  setDoc,
  startAfter,
  startAt,
  Timestamp,
  updateDoc,
  where,
  writeBatch,
  type Firestore,
} from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";

export const createRestFirestoreAdapter = (
  firestore: Firestore,
): { instance: Firestore; operations: FirestoreOperations } => {
  const operations: FirestoreOperations = {
    doc,
    collection,
    getDoc,
    getDocs,
    setDoc,
    updateDoc,
    deleteDoc,
    addDoc,
    query,
    where,
    orderBy,
    limit,
    limitToLast,
    startAt,
    startAfter,
    endAt,
    endBefore,
    and,
    or,
    collectionGroup,
    runTransaction,
    writeBatch,
    createTimestamp: (date: Date) => Timestamp.fromDate(date),
  };

  return { instance: firestore, operations };
};
