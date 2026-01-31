/**
 * Admin Feature Test Setup
 *
 * Firebase Admin SDKを使用してFeature Testを実行するための設定。
 *
 * 前提条件:
 * - Firebase Emulatorが起動していること (docker-compose up -d)
 * - Firestore Emulator: localhost:8085
 * - Auth Emulator: localhost:9099
 */

import {
  getApps,
  initializeApp,
  deleteApp,
  type App,
} from "firebase-admin/app";
import { getFirestore, type Firestore } from "firebase-admin/firestore";
import { Logger, Environment } from "@shared/aspects/logger";

const EMULATOR_CONFIG = {
  firestoreHost: "localhost",
  firestorePort: 8085,
  authHost: "localhost",
  authPort: 9099,
};

const TEST_APP_NAME = "admin-feature-test-app";
const TEST_PROJECT_ID = "demo-hut";

let appInstance: App | null = null;
let firestoreInstance: Firestore | null = null;

/**
 * エミュレータ環境変数を設定
 */
function ensureEmulatorEnvironment(): void {
  if (!process.env.FIRESTORE_EMULATOR_HOST) {
    process.env.FIRESTORE_EMULATOR_HOST = `${EMULATOR_CONFIG.firestoreHost}:${EMULATOR_CONFIG.firestorePort}`;
  }
  if (!process.env.FIREBASE_AUTH_EMULATOR_HOST) {
    process.env.FIREBASE_AUTH_EMULATOR_HOST = `${EMULATOR_CONFIG.authHost}:${EMULATOR_CONFIG.authPort}`;
  }
}

/**
 * Firebase Admin SDKのアプリインスタンスを取得
 */
function getTestAdminApp(): App {
  if (appInstance) {
    return appInstance;
  }

  ensureEmulatorEnvironment();

  const existingApps = getApps();
  const existingTestApp = existingApps.find((app) => app.name === TEST_APP_NAME);

  if (existingTestApp) {
    appInstance = existingTestApp;
    return appInstance;
  }

  appInstance = initializeApp(
    { projectId: TEST_PROJECT_ID },
    TEST_APP_NAME
  );

  return appInstance;
}

/**
 * Firebase Admin Firestoreインスタンスを取得
 */
function getTestFirestore(): Firestore {
  if (firestoreInstance) {
    return firestoreInstance;
  }

  firestoreInstance = getFirestore(getTestAdminApp());
  return firestoreInstance;
}

/**
 * 指定されたコレクションのドキュメントをすべて削除
 */
async function clearCollection(
  firestore: Firestore,
  collectionPath: string
): Promise<void> {
  const collectionRef = firestore.collection(collectionPath);
  const snapshot = await collectionRef.get();

  const batch = firestore.batch();
  snapshot.docs.forEach((document) => {
    batch.delete(document.ref);
  });

  if (snapshot.docs.length > 0) {
    await batch.commit();
  }
}

/**
 * テストで使用するコレクションをすべてクリア
 */
async function clearAllTestData(firestore: Firestore): Promise<void> {
  const collectionsToClean = [
    "articles",
    "memos",
    "series",
    "tags",
    "admin",
    "site-documents",
    "admin-rate-limits",
    "index/articles/slug",
    "index/memos/slug",
    "index/series/slug",
    "index/tags/name",
  ];

  await Promise.all(
    collectionsToClean.map((collectionPath) =>
      clearCollection(firestore, collectionPath)
    )
  );
}

/**
 * Feature Test用のロガー
 */
export const testLogger = Logger(Environment.DEVELOPMENT);

export type AdminFeatureTestContext = {
  firestore: Firestore;
  cleanup: () => Promise<void>;
};

/**
 * Admin Feature Test用のコンテキストを作成
 */
export async function createAdminFeatureTestContext(): Promise<AdminFeatureTestContext> {
  const firestore = getTestFirestore();

  await clearAllTestData(firestore);

  return {
    firestore,
    cleanup: async () => {
      await clearAllTestData(firestore);
    },
  };
}

/**
 * テスト終了時のクリーンアップ
 */
export async function cleanupAdminFeatureTest(): Promise<void> {
  if (firestoreInstance) {
    await firestoreInstance.terminate();
    firestoreInstance = null;
  }

  if (appInstance) {
    await deleteApp(appInstance);
    appInstance = null;
  }
}

export { clearAllTestData, clearCollection, getTestFirestore };
