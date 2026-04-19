import {
  getApp,
  getApps,
  initializeApp,
  type FirebaseApp,
} from "firebase/app";
import {
  connectFirestoreEmulator,
  getFirestore,
  type Firestore,
} from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";
import { createRestFirestoreAdapter } from "@shared/infrastructures/rest/firestore-rest-adapter";

const useEmulator = process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true";
const emulatorHost = "localhost";
const firestoreEmulatorPort = 8085;

let appInstance: FirebaseApp | null = null;
let firestoreInstance: Firestore | null = null;
let cachedAdapter: {
  instance: Firestore;
  operations: FirestoreOperations;
} | null = null;

const resolveProjectId = (): string => {
  if (useEmulator) {
    return "demo-hut";
  }
  const projectId = process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID;
  if (!projectId) {
    throw new Error(
      "NEXT_PUBLIC_FIREBASE_PROJECT_ID is required for Firebase REST client",
    );
  }
  return projectId;
};

const resolveApiKey = (): string => {
  if (useEmulator) {
    return "fake-api-key";
  }
  const apiKey = process.env.NEXT_PUBLIC_FIREBASE_API_KEY;
  if (!apiKey) {
    throw new Error(
      "NEXT_PUBLIC_FIREBASE_API_KEY is required for Firebase REST client",
    );
  }
  return apiKey;
};

const resolveDatabaseId = (): string | undefined =>
  process.env.NEXT_PUBLIC_FIREBASE_DATABASE_ID;

const getRestApp = (): FirebaseApp => {
  if (appInstance !== null) {
    return appInstance;
  }
  if (getApps().length > 0) {
    appInstance = getApp();
    return appInstance;
  }
  appInstance = initializeApp({
    projectId: resolveProjectId(),
    apiKey: resolveApiKey(),
  });
  return appInstance;
};

const getFirestoreInstance = (): Firestore => {
  if (firestoreInstance !== null) {
    return firestoreInstance;
  }
  const databaseId = resolveDatabaseId();
  const app = getRestApp();
  firestoreInstance =
    databaseId !== undefined
      ? getFirestore(app, databaseId)
      : getFirestore(app);

  if (useEmulator) {
    connectFirestoreEmulator(
      firestoreInstance,
      emulatorHost,
      firestoreEmulatorPort,
    );
  }

  return firestoreInstance;
};

const getAdapter = (): {
  instance: Firestore;
  operations: FirestoreOperations;
} => {
  if (cachedAdapter !== null) {
    return cachedAdapter;
  }
  cachedAdapter = createRestFirestoreAdapter(getFirestoreInstance());
  return cachedAdapter;
};

export const FirebaseRestProvider = {
  firestore: {
    get instance(): Firestore {
      return getAdapter().instance;
    },
    get operations(): FirestoreOperations {
      return getAdapter().operations;
    },
  },
} as const;
