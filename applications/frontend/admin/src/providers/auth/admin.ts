import {
  applicationDefault,
  getApp,
  getApps,
  initializeApp,
  type App,
} from "firebase-admin/app";
import { getAuth, type Auth } from "firebase-admin/auth";
import { getFirestore, type Firestore } from "firebase-admin/firestore";
import { getStorage } from "firebase-admin/storage";
import type { Bucket } from "@google-cloud/storage";

const useEmulator = process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true";
const emulatorHost = "localhost";
const emulatorPort = 9099;
const firestoreEmulatorPort = 8085;
const storageEmulatorPort = 9199;

let authInstance: Auth | null = null;
let firestoreInstance: Firestore | null = null;
let storageBucketInstance: Bucket | null = null;

const resolveProjectId = (): string => {
  if (useEmulator) {
    return "demo-hut";
  }

  const projectId =
    process.env.FIREBASE_PROJECT_ID ??
    process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID;

  if (!projectId) {
    throw new Error("FIREBASE_PROJECT_ID is required for Firebase Admin");
  }

  return projectId;
};

const ensureAuthEmulator = (): void => {
  if (!useEmulator) {
    return;
  }

  if (!process.env.FIREBASE_AUTH_EMULATOR_HOST) {
    process.env.FIREBASE_AUTH_EMULATOR_HOST = `${emulatorHost}:${emulatorPort}`;
  }
};

const ensureFirestoreEmulator = (): void => {
  if (!useEmulator) {
    return;
  }

  if (!process.env.FIRESTORE_EMULATOR_HOST) {
    process.env.FIRESTORE_EMULATOR_HOST = `${emulatorHost}:${firestoreEmulatorPort}`;
  }
};

const ensureStorageEmulator = (): void => {
  if (!useEmulator) {
    return;
  }

  if (!process.env.FIREBASE_STORAGE_EMULATOR_HOST) {
    process.env.FIREBASE_STORAGE_EMULATOR_HOST = `${emulatorHost}:${storageEmulatorPort}`;
  }
};

const getFirebaseAdminApp = (): App => {
  if (getApps().length > 0) {
    return getApp();
  }

  ensureAuthEmulator();

  const projectId = resolveProjectId();

  return initializeApp(
    useEmulator
      ? { projectId }
      : {
          projectId,
          credential: applicationDefault(),
        },
  );
};

const getAuthInstance = (): Auth => {
  if (authInstance === null) {
    authInstance = getAuth(getFirebaseAdminApp());
  }

  return authInstance;
};

const getFirestoreInstance = (): Firestore => {
  if (firestoreInstance === null) {
    ensureFirestoreEmulator();
    firestoreInstance = getFirestore(getFirebaseAdminApp());
  }

  return firestoreInstance;
};

const getStorageBucketInstance = (): Bucket => {
  if (storageBucketInstance === null) {
    ensureStorageEmulator();
    storageBucketInstance = getStorage(getFirebaseAdminApp()).bucket(
      process.env.NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET,
    );
  }

  return storageBucketInstance;
};

export const FirebaseAdminProvider = {
  auth: {
    get instance(): Auth {
      return getAuthInstance();
    },
  },
  firestore: {
    get instance(): Firestore {
      return getFirestoreInstance();
    },
  },
  storage: {
    get instance(): Bucket {
      return getStorageBucketInstance();
    },
  },
} as const;
