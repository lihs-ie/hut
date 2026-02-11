import { initializeApp, deleteApp, type FirebaseApp, getApps } from "firebase/app";
import {
  getFirestore,
  connectFirestoreEmulator,
  collection,
  collectionGroup,
  getDocs,
  deleteDoc,
  doc,
  addDoc,
  getDoc,
  setDoc,
  updateDoc,
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
  runTransaction,
  writeBatch,
  Timestamp,
  type Firestore,
  terminate,
} from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { Logger, Environment } from "@shared/aspects/logger";

const EMULATOR_CONFIG = {
  firestoreHost: "localhost",
  firestorePort: 8085,
};

const firebaseConfig = {
  apiKey: "demo-api-key",
  authDomain: "demo-hut.firebaseapp.com",
  projectId: "demo-hut",
  storageBucket: "demo-hut.appspot.com",
  messagingSenderId: "000000000000",
  appId: "demo-app-id",
};

let appInstance: FirebaseApp | null = null;
let firestoreInstance: Firestore | null = null;
let emulatorConnected = false;

const TEST_APP_NAME = "feature-test-app";

function getTestFirestore(): { firestore: Firestore; app: FirebaseApp } {
  if (appInstance && firestoreInstance) {
    return { firestore: firestoreInstance, app: appInstance };
  }

  const existingApps = getApps();
  const existingTestApp = existingApps.find((app) => app.name === TEST_APP_NAME);

  if (existingTestApp) {
    appInstance = existingTestApp;
  } else {
    appInstance = initializeApp(firebaseConfig, TEST_APP_NAME);
  }

  firestoreInstance = getFirestore(appInstance);

  if (!emulatorConnected) {
    connectFirestoreEmulator(
      firestoreInstance,
      EMULATOR_CONFIG.firestoreHost,
      EMULATOR_CONFIG.firestorePort
    );
    emulatorConnected = true;
  }

  return { firestore: firestoreInstance, app: appInstance };
}

function getTestOperations(): FirestoreOperations {
  return {
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
}

async function clearCollection(
  firestore: Firestore,
  collectionPath: string
): Promise<void> {
  const collectionRef = collection(firestore, collectionPath);
  const snapshot = await getDocs(collectionRef);

  const batch = writeBatch(firestore);
  snapshot.docs.forEach((document) => {
    batch.delete(document.ref);
  });

  if (snapshot.docs.length > 0) {
    await batch.commit();
  }
}

async function clearNestedCollection(
  firestore: Firestore,
  rootCollection: string,
  subcollectionNames: string[]
): Promise<void> {
  const rootCollectionRef = collection(firestore, rootCollection);
  const typeSnapshot = await getDocs(rootCollectionRef);

  for (const typeDoc of typeSnapshot.docs) {
    for (const subcollectionName of subcollectionNames) {
      const subRef = collection(typeDoc.ref, subcollectionName);
      const subSnapshot = await getDocs(subRef);
      for (const subDoc of subSnapshot.docs) {
        await deleteDoc(subDoc.ref);
      }
    }
    await deleteDoc(typeDoc.ref);
  }
}

async function clearSearchTokens(firestore: Firestore): Promise<void> {
  const tokensRef = collection(firestore, "search-tokens");
  const tokenSnapshot = await getDocs(tokensRef);

  for (const tokenDoc of tokenSnapshot.docs) {
    const refsRef = collection(tokenDoc.ref, "refs");
    const refsSnapshot = await getDocs(refsRef);

    const batch = writeBatch(firestore);
    refsSnapshot.docs.forEach((refDoc) => {
      batch.delete(refDoc.ref);
    });
    if (refsSnapshot.docs.length > 0) {
      await batch.commit();
    }

    await deleteDoc(tokenDoc.ref);
  }
}

async function clearAllTestData(firestore: Firestore): Promise<void> {
  const collectionsToClean = [
    "articles",
    "memos",
    "series",
    "tags",
    "admin",
    "site-documents",
    "search-index",
    "index/articles/slug",
    "index/memos/slug",
    "index/series/slug",
    "index/tags/name",
  ];

  const analyticsCollections = [
    "unique-visitor-counters",
    "unique-visitor-dedup",
  ];

  const nestedCollectionConfig: Array<{ root: string; subcollections: string[] }> = [
    { root: "page-view-counters", subcollections: [] },
    { root: "page-view-dedup", subcollections: [] },
    { root: "access-logs", subcollections: [] },
    { root: "search-logs", subcollections: ["records"] },
    { root: "engagement-logs", subcollections: [] },
  ];

  await Promise.all([
    ...collectionsToClean.map((collectionPath) =>
      clearCollection(firestore, collectionPath)
    ),
    ...nestedCollectionConfig.map((config) =>
      clearNestedCollection(firestore, config.root, config.subcollections)
    ),
    ...analyticsCollections.map((collectionPath) =>
      clearCollection(firestore, collectionPath)
    ),
    clearSearchTokens(firestore),
  ]);
}

export const testLogger = Logger(Environment.DEVELOPMENT);

export type FeatureTestContext = {
  firestore: Firestore;
  operations: FirestoreOperations;
  cleanup: () => Promise<void>;
};

async function clearEmulatorData(): Promise<void> {
  try {
    await fetch(
      `http://${EMULATOR_CONFIG.firestoreHost}:${EMULATOR_CONFIG.firestorePort}/emulator/v1/projects/demo-hut/databases/(default)/documents`,
      { method: "DELETE" },
    );
  } catch {
    return;
  }
}

export async function createFeatureTestContext(): Promise<FeatureTestContext> {
  const { firestore } = getTestFirestore();
  const operations = getTestOperations();

  await clearEmulatorData();

  return {
    firestore,
    operations,
    cleanup: async () => {
      await clearEmulatorData();
    },
  };
}

export async function cleanupFeatureTest(): Promise<void> {
  if (firestoreInstance) {
    await terminate(firestoreInstance);
    firestoreInstance = null;
  }

  if (appInstance) {
    await deleteApp(appInstance);
    appInstance = null;
  }

  emulatorConnected = false;
}

export { clearAllTestData, clearCollection };
