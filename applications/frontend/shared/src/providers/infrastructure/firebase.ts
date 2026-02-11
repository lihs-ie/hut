import { initializeApp, getApps, getApp, type FirebaseApp } from "firebase/app";
import {
  getFirestore,
  connectFirestoreEmulator,
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
import {
  getStorage,
  connectStorageEmulator,
  type FirebaseStorage,
} from "firebase/storage";

import { type FirestoreOperations } from "@shared/infrastructures/common";

const useEmulator = process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true";

const firebaseConfig = {
  apiKey: useEmulator
    ? "demo-api-key"
    : process.env.NEXT_PUBLIC_FIREBASE_API_KEY,
  authDomain: useEmulator
    ? "demo-hut.firebaseapp.com"
    : process.env.NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN,
  projectId: useEmulator
    ? "demo-hut"
    : process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID,
  storageBucket: useEmulator
    ? "demo-hut.appspot.com"
    : process.env.NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET,
  messagingSenderId: useEmulator
    ? "000000000000"
    : process.env.NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID,
  appId: useEmulator ? "demo-app-id" : process.env.NEXT_PUBLIC_FIREBASE_APP_ID,
};

const getFirebaseApp = (): FirebaseApp => {
  if (getApps().length > 0) {
    return getApp();
  }
  return initializeApp(firebaseConfig);
};

const emulatorConfig = {
  firestoreHost: "localhost",
  firestorePort: 8085,
  storageHost: "localhost",
  storagePort: 9199,
};

let firestoreInstance: Firestore | null = null;
let storageInstance: FirebaseStorage | null = null;
let firestoreEmulatorConnected = false;
let storageEmulatorConnected = false;

const getFirestoreInstance = (): Firestore => {
  if (firestoreInstance === null) {
    const app = getFirebaseApp();

    firestoreInstance = getFirestore(app);

    if (useEmulator && !firestoreEmulatorConnected) {
      connectFirestoreEmulator(
        firestoreInstance,
        emulatorConfig.firestoreHost,
        emulatorConfig.firestorePort,
      );
      firestoreEmulatorConnected = true;
    }
  }

  return firestoreInstance;
};

const getStorageInstance = (): FirebaseStorage => {
  if (storageInstance === null) {
    const app = getFirebaseApp();

    storageInstance = getStorage(app);

    if (useEmulator && !storageEmulatorConnected) {
      connectStorageEmulator(
        storageInstance,
        emulatorConfig.storageHost,
        emulatorConfig.storagePort,
      );
      storageEmulatorConnected = true;
    }
  }

  return storageInstance;
};

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

export const FirebaseProvider = {
  firestore: {
    instance: getFirestoreInstance(),
    operations,
  },
  storage: {
    instance: getStorageInstance(),
  },
} as const;
