/**
 * Firebase Test Utilities
 *
 * Feature Testで使用するFirebase関連のユーティリティ。
 * 各テストファイルで重複していたセットアップコードを共通化。
 */

import { initializeApp, getApps, deleteApp, FirebaseApp } from "firebase/app";
import {
  getFirestore,
  connectFirestoreEmulator,
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
  Timestamp,
  type Firestore,
} from "firebase/firestore";
import type { FirestoreOperations } from "@shared/infrastructures/common";

const TEST_PROJECT_ID = "demo-hut";

const EMULATOR_CONFIG = {
  firestoreHost: "localhost",
  firestorePort: 8085,
};

type FirebaseTestState = {
  appInstance: FirebaseApp | null;
  firestoreInstance: Firestore | null;
  emulatorConnected: boolean;
};

const stateByAppName = new Map<string, FirebaseTestState>();

function getState(appName: string): FirebaseTestState {
  if (!stateByAppName.has(appName)) {
    stateByAppName.set(appName, {
      appInstance: null,
      firestoreInstance: null,
      emulatorConnected: false,
    });
  }
  return stateByAppName.get(appName)!;
}

function ensureEmulatorEnvironment(): void {
  if (!process.env.FIRESTORE_EMULATOR_HOST) {
    process.env.FIRESTORE_EMULATOR_HOST = `${EMULATOR_CONFIG.firestoreHost}:${EMULATOR_CONFIG.firestorePort}`;
  }
}

export function getTestApp(appName: string): FirebaseApp {
  const state = getState(appName);

  if (state.appInstance) {
    return state.appInstance;
  }

  ensureEmulatorEnvironment();

  const existingApps = getApps();
  const existingTestApp = existingApps.find(
    (application) => application.name === appName,
  );

  if (existingTestApp) {
    state.appInstance = existingTestApp;
    return state.appInstance;
  }

  state.appInstance = initializeApp(
    {
      apiKey: "demo-api-key",
      projectId: TEST_PROJECT_ID,
    },
    appName,
  );

  return state.appInstance;
}

export function getTestFirestoreInstance(appName: string): Firestore {
  const state = getState(appName);

  if (state.firestoreInstance) {
    return state.firestoreInstance;
  }

  state.firestoreInstance = getFirestore(getTestApp(appName));

  if (!state.emulatorConnected) {
    connectFirestoreEmulator(
      state.firestoreInstance,
      EMULATOR_CONFIG.firestoreHost,
      EMULATOR_CONFIG.firestorePort,
    );
    state.emulatorConnected = true;
  }

  return state.firestoreInstance;
}

export function createFirestoreOperations(): FirestoreOperations {
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

export async function cleanupTestApp(appName: string): Promise<void> {
  const state = getState(appName);

  if (state.appInstance) {
    await deleteApp(state.appInstance);
    state.appInstance = null;
    state.firestoreInstance = null;
    state.emulatorConnected = false;
  }

  stateByAppName.delete(appName);
}

export { Timestamp };
