/**
 * Firestore Emulator シードスクリプト共通ユーティリティ
 */

import { ulid } from "ulid";

// Firestore Emulator 接続設定
export const FIRESTORE_EMULATOR_HOST = "http://localhost:8085";
export const PROJECT_ID = "demo-hut";
export const DATABASE_ID = "(default)";
export const BASE_URL = `${FIRESTORE_EMULATOR_HOST}/v1/projects/${PROJECT_ID}/databases/${DATABASE_ID}/documents`;

// テストデータ用のULID
export const TAG_IDS = {
  // Frontend
  typescript: ulid(),
  javascript: ulid(),
  react: ulid(),
  nextjs: ulid(),
  vue: ulid(),
  nuxt: ulid(),
  angular: ulid(),
  svelte: ulid(),
  tailwindcss: ulid(),
  css: ulid(),
  html: ulid(),
  // Backend
  nodejs: ulid(),
  go: ulid(),
  rust: ulid(),
  python: ulid(),
  java: ulid(),
  php: ulid(),
  ruby: ulid(),
  csharp: ulid(),
  kotlin: ulid(),
  // Database
  postgresql: ulid(),
  mysql: ulid(),
  mongodb: ulid(),
  redis: ulid(),
};

export const ARTICLE_IDS = {
  article1: ulid(),
  article2: ulid(),
  article3: ulid(),
  article4: ulid(),
};

export const SERIES_IDS = {
  series1: ulid(),
};

export const MEMO_IDS = {
  memo1: ulid(),
  memo2: ulid(),
};

export const ADMIN_ID = ulid();

// N-gram生成関数
export const normalize = (text: string): string =>
  text
    .toLowerCase()
    .replace(/\s+/g, "")
    .replace(/[^\p{L}\p{N}]/gu, "");

export const generateNgrams = (text: string, min = 2, max = 4): string[] => {
  const normalized = normalize(text);
  const ngrams: Set<string> = new Set();

  for (let n = min; n <= max; n++) {
    for (let i = 0; i <= normalized.length - n; i++) {
      ngrams.add(normalized.slice(i, i + n));
    }
  }

  return Array.from(ngrams);
};

// Firestore REST API用のデータ変換
// useTimestamp: trueの場合はtimestampValue、falseの場合はstringValueとして保存
export function toFirestoreValue(
  value: unknown,
  useTimestamp = false,
): Record<string, unknown> {
  if (value === null) {
    return { nullValue: null };
  }
  if (typeof value === "string") {
    return { stringValue: value };
  }
  if (typeof value === "number") {
    if (Number.isInteger(value)) {
      return { integerValue: value.toString() };
    }
    return { doubleValue: value };
  }
  if (typeof value === "boolean") {
    return { booleanValue: value };
  }
  if (value instanceof Date) {
    // インフラ層ではtimelineをISO文字列として保存しているため、stringValueを使用
    if (useTimestamp) {
      return { timestampValue: value.toISOString() };
    }
    return { stringValue: value.toISOString() };
  }
  if (Array.isArray(value)) {
    return {
      arrayValue: {
        values: value.map((item) => toFirestoreValue(item, useTimestamp)),
      },
    };
  }
  if (value instanceof Map) {
    const fields: Record<string, unknown> = {};
    value.forEach((v, k) => {
      fields[k] = toFirestoreValue(v, useTimestamp);
    });
    return { mapValue: { fields } };
  }
  if (typeof value === "object") {
    const fields: Record<string, unknown> = {};
    for (const [k, v] of Object.entries(value)) {
      fields[k] = toFirestoreValue(v, useTimestamp);
    }
    return { mapValue: { fields } };
  }
  return { stringValue: String(value) };
}

export function toFirestoreDocument(
  data: Record<string, unknown>,
  useTimestamp = false,
): Record<string, unknown> {
  const fields: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(data)) {
    fields[key] = toFirestoreValue(value, useTimestamp);
  }
  return { fields };
}

export type CreateDocumentOptions = {
  useTimestamp?: boolean;
};

export async function createDocument(
  collection: string,
  documentId: string,
  data: Record<string, unknown>,
  options: CreateDocumentOptions = {},
): Promise<void> {
  const url = `${BASE_URL}/${collection}?documentId=${documentId}`;
  const body = toFirestoreDocument(data, options.useTimestamp ?? false);

  const response = await fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(body),
  });

  if (!response.ok) {
    const error = await response.text();
    throw new Error(`Failed to create ${collection}/${documentId}: ${error}`);
  }

  console.log(`Created ${collection}/${documentId}`);
}

// サブコレクションにドキュメントを作成
export async function createSubDocument(
  parentCollection: string,
  parentDocumentId: string,
  subCollection: string,
  documentId: string,
  data: Record<string, unknown>,
  options: CreateDocumentOptions = {},
): Promise<void> {
  const url = `${BASE_URL}/${parentCollection}/${parentDocumentId}/${subCollection}?documentId=${documentId}`;
  const body = toFirestoreDocument(data, options.useTimestamp ?? false);

  const response = await fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(body),
  });

  if (!response.ok) {
    const error = await response.text();
    throw new Error(
      `Failed to create ${parentCollection}/${parentDocumentId}/${subCollection}/${documentId}: ${error}`,
    );
  }

  console.log(
    `Created ${parentCollection}/${parentDocumentId}/${subCollection}/${documentId}`,
  );
}

// 既存データの削除
export async function clearCollection(collection: string): Promise<void> {
  // const url = `${BASE_URL}/${collection}`;
  // const response = await fetch(url);

  // if (!response.ok) {
  //   console.log(`No existing ${collection} to clear`);
  //   return;
  // }

  // const data = (await response.json()) as {
  //   documents?: Array<{ name: string }>;
  // };
  // const documents = data.documents || [];

  // for (const doc of documents) {
  //   const deleteUrl = `${FIRESTORE_EMULATOR_HOST}/v1/${doc.name}`;
  //   await fetch(deleteUrl, { method: "DELETE" });
  //   console.log(`Deleted ${doc.name}`);
  // }

  const pageSize = 100;

  while (true) {
    const url = new URL(`${BASE_URL}/${collection}`);
    url.searchParams.set("pageSize", String(pageSize));

    const response = await fetch(url.toString());
    if (!response.ok) {
      console.log(`No existing ${collection} to clear`);
      return;
    }

    const data = (await response.json()) as {
      documents?: Array<{ name: string }>;
    };

    const documents = data.documents ?? [];
    if (documents.length === 0) {
      return;
    }

    for (const doc of documents) {
      const deleteUrl = `${FIRESTORE_EMULATOR_HOST}/v1/${doc.name}`;
      await fetch(deleteUrl, { method: "DELETE" });
      console.log(`Deleted ${doc.name}`);
    }
  }
}
