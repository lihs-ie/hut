import {
  applicationDefault,
  getApp,
  getApps,
  initializeApp,
  type App,
} from "firebase-admin/app";
import { getAuth, type Auth } from "firebase-admin/auth";
import { getFirestore, type Firestore } from "firebase-admin/firestore";

const useEmulator = process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true";
const emulatorHost = "localhost";
const emulatorPort = 9099;
const firestoreEmulatorPort = 8085;

let authInstance: Auth | null = null;
let firestoreInstance: Firestore | null = null;

/**
 * Resolve the Firebase project ID for Admin SDK.
 */
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

/**
 * Ensure Auth emulator host is configured.
 */
const ensureAuthEmulator = (): void => {
  if (!useEmulator) {
    return;
  }

  if (!process.env.FIREBASE_AUTH_EMULATOR_HOST) {
    process.env.FIREBASE_AUTH_EMULATOR_HOST = `${emulatorHost}:${emulatorPort}`;
  }
};

/**
 * Ensure Firestore emulator host is configured.
 */
const ensureFirestoreEmulator = (): void => {
  if (!useEmulator) {
    return;
  }

  if (!process.env.FIRESTORE_EMULATOR_HOST) {
    process.env.FIRESTORE_EMULATOR_HOST = `${emulatorHost}:${firestoreEmulatorPort}`;
  }
};

/**
 * Create or reuse the Firebase Admin app instance.
 */
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

/**
 * Get the Firebase Admin Auth instance.
 */
const getAuthInstance = (): Auth => {
  if (authInstance === null) {
    authInstance = getAuth(getFirebaseAdminApp());
  }

  return authInstance;
};

/**
 * Get the Firebase Admin Firestore instance.
 */
const getFirestoreInstance = (): Firestore => {
  if (firestoreInstance === null) {
    ensureFirestoreEmulator();
    firestoreInstance = getFirestore(getFirebaseAdminApp());
  }

  return firestoreInstance;
};

export const FirebaseAdminProvider = {
  auth: {
    instance: getAuthInstance(),
  },
  firestore: {
    instance: getFirestoreInstance(),
  },
} as const;
