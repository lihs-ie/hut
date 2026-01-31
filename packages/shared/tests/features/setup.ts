/**
 * Feature Test Setup
 *
 * Firebase Emulatorに接続してFeature Testを実行するための設定。
 *
 * 前提条件:
 * - Firebase Emulatorが起動していること (docker-compose up -d)
 * - Firestore Emulator: localhost:8085
 */

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

/**
 * Firebase Emulatorに接続したFirestoreインスタンスを取得
 */
function getTestFirestore(): { firestore: Firestore; app: FirebaseApp } {
  if (appInstance && firestoreInstance) {
    return { firestore: firestoreInstance, app: appInstance };
  }

  // テスト専用のアプリ名で検索
  const existingApps = getApps();
  const existingTestApp = existingApps.find((app) => app.name === TEST_APP_NAME);

  if (existingTestApp) {
    appInstance = existingTestApp;
  } else {
    // テスト専用の新しいアプリを作成
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

/**
 * Firestore Operationsの取得
 */
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

/**
 * 指定されたコレクションのドキュメントをすべて削除
 */
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

/**
 * ネストされたドキュメントを削除
 * page-view-counters/{type}/{content}/{dateKey} のようなパスに対応
 * Firestoreの構造: page-view-counters は親コレクション
 */
async function clearNestedPageViewData(
  firestore: Firestore,
  rootCollection: string
): Promise<void> {
  // ルートコレクションを取得
  const rootCollectionRef = collection(firestore, rootCollection);
  const typeSnapshot = await getDocs(rootCollectionRef);

  for (const typeDoc of typeSnapshot.docs) {
    // typeDoc.ref は page-view-counters/{type} ドキュメント
    // その下のサブコレクション（content identifiers）を取得
    // しかしFirestoreではサブコレクションを一覧で取得する標準方法がない
    // ここではドキュメント削除のみ行う
    await deleteDoc(typeDoc.ref);
  }
}

/**
 * search-tokensコレクションとそのサブコレクション(refs)を削除
 */
async function clearSearchTokens(firestore: Firestore): Promise<void> {
  const tokensRef = collection(firestore, "search-tokens");
  const tokenSnapshot = await getDocs(tokensRef);

  for (const tokenDoc of tokenSnapshot.docs) {
    // サブコレクション (refs) を削除
    const refsRef = collection(tokenDoc.ref, "refs");
    const refsSnapshot = await getDocs(refsRef);

    const batch = writeBatch(firestore);
    refsSnapshot.docs.forEach((refDoc) => {
      batch.delete(refDoc.ref);
    });
    if (refsSnapshot.docs.length > 0) {
      await batch.commit();
    }

    // 親ドキュメントを削除
    await deleteDoc(tokenDoc.ref);
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
    "search-index",
    "index/articles/slug",
    "index/memos/slug",
    "index/series/slug",
    "index/tags/name",
  ];

  // ネストされたページビュー関連コレクション
  const nestedCollections = [
    "page-view-counters",
    "page-view-dedup",
  ];

  await Promise.all([
    ...collectionsToClean.map((collectionPath) =>
      clearCollection(firestore, collectionPath)
    ),
    ...nestedCollections.map((rootCollection) =>
      clearNestedPageViewData(firestore, rootCollection)
    ),
    clearSearchTokens(firestore),
  ]);
}

/**
 * Feature Test用のロガー（共通利用）
 */
export const testLogger = Logger(Environment.DEVELOPMENT);

export type FeatureTestContext = {
  firestore: Firestore;
  operations: FirestoreOperations;
  cleanup: () => Promise<void>;
};

/**
 * Feature Test用のコンテキストを作成
 */
export async function createFeatureTestContext(): Promise<FeatureTestContext> {
  const { firestore } = getTestFirestore();
  const operations = getTestOperations();

  // テスト開始前にデータをクリア
  await clearAllTestData(firestore);

  return {
    firestore,
    operations,
    cleanup: async () => {
      await clearAllTestData(firestore);
    },
  };
}

/**
 * テスト終了時のクリーンアップ
 */
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
