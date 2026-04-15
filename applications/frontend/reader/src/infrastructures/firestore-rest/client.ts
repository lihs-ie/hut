import { firestoreFieldsToObject, type JsonValue } from "./value-converter";
import type { AccessTokenProvider } from "./access-token";

const DEFAULT_FIRESTORE_BASE_URL = "https://firestore.googleapis.com/v1";
const EMULATOR_OWNER_TOKEN = "owner";

export type FirestoreClientConfig = {
  projectId: string;
  databaseId?: string;
  baseUrl?: string;
  accessTokenProvider?: AccessTokenProvider;
};

export type FirestoreQueryValue =
  | { stringValue: string }
  | { integerValue: number }
  | { booleanValue: boolean }
  | { timestampValue: string };

export type FirestoreFieldFilter = {
  field: string;
  op: "EQUAL" | "LESS_THAN" | "LESS_THAN_OR_EQUAL" | "GREATER_THAN" | "GREATER_THAN_OR_EQUAL" | "ARRAY_CONTAINS" | "ARRAY_CONTAINS_ANY" | "IN";
  value: FirestoreQueryValue | { arrayValue: { values: FirestoreQueryValue[] } };
};

export type FirestoreOrderBy = {
  field: string;
  direction: "ASCENDING" | "DESCENDING";
};

export type FirestoreQueryInput = {
  collectionId: string;
  where: FirestoreFieldFilter[];
  orderBy?: FirestoreOrderBy[];
  limit?: number;
};

export type FirestoreRestClient = {
  getDocument: (path: string) => Promise<Record<string, JsonValue> | null>;
  runQuery: (input: FirestoreQueryInput) => Promise<Record<string, JsonValue>[]>;
};

type DocumentResponse = {
  name?: string;
  fields?: Record<string, unknown>;
};

type RunQueryEntry = {
  document?: DocumentResponse;
};

const isPlainObject = (
  value: unknown,
): value is Record<string, unknown> => {
  return value !== null && typeof value === "object" && !Array.isArray(value);
};

const isDocumentResponse = (value: unknown): value is DocumentResponse => {
  if (!isPlainObject(value)) {
    return false;
  }
  if (value.name !== undefined && typeof value.name !== "string") {
    return false;
  }
  if (value.fields !== undefined && !isPlainObject(value.fields)) {
    return false;
  }
  return true;
};

const isRunQueryEntry = (value: unknown): value is RunQueryEntry => {
  if (!isPlainObject(value)) {
    return false;
  }
  if (value.document !== undefined && !isDocumentResponse(value.document)) {
    return false;
  }
  return true;
};

const buildDocumentUrl = (
  baseUrl: string,
  projectId: string,
  databaseId: string,
  path: string,
): string =>
  `${baseUrl}/projects/${projectId}/databases/${databaseId}/documents/${path}`;

const buildRunQueryUrl = (
  baseUrl: string,
  projectId: string,
  databaseId: string,
): string =>
  `${baseUrl}/projects/${projectId}/databases/${databaseId}/documents:runQuery`;

const buildStructuredQuery = (input: FirestoreQueryInput): Record<string, unknown> => {
  const structuredQuery: Record<string, unknown> = {
    from: [{ collectionId: input.collectionId }],
  };

  if (input.where.length === 1) {
    const [filter] = input.where;
    structuredQuery.where = {
      fieldFilter: {
        field: { fieldPath: filter.field },
        op: filter.op,
        value: filter.value,
      },
    };
  } else if (input.where.length > 1) {
    structuredQuery.where = {
      compositeFilter: {
        op: "AND",
        filters: input.where.map((filter) => ({
          fieldFilter: {
            field: { fieldPath: filter.field },
            op: filter.op,
            value: filter.value,
          },
        })),
      },
    };
  }

  if (input.orderBy !== undefined && input.orderBy.length > 0) {
    structuredQuery.orderBy = input.orderBy.map((item) => ({
      field: { fieldPath: item.field },
      direction: item.direction,
    }));
  }

  if (typeof input.limit === "number") {
    structuredQuery.limit = input.limit;
  }

  return { structuredQuery };
};

const toDocument = (
  response: unknown,
): Record<string, JsonValue> | null => {
  if (!isDocumentResponse(response)) {
    return null;
  }
  return firestoreFieldsToObject(response.fields);
};

export const createFirestoreRestClient = (
  config: FirestoreClientConfig,
): FirestoreRestClient => {
  const databaseId = config.databaseId ?? "(default)";
  const baseUrl = config.baseUrl ?? DEFAULT_FIRESTORE_BASE_URL;
  const accessTokenProvider = config.accessTokenProvider;

  const resolveAuthorization = async (): Promise<string> => {
    if (accessTokenProvider === undefined) {
      return `Bearer ${EMULATOR_OWNER_TOKEN}`;
    }
    const accessToken = await accessTokenProvider.getAccessToken();
    return `Bearer ${accessToken}`;
  };

  const getDocument: FirestoreRestClient["getDocument"] = async (path) => {
    const authorization = await resolveAuthorization();
    const response = await fetch(
      buildDocumentUrl(baseUrl, config.projectId, databaseId, path),
      {
        method: "GET",
        headers: {
          Authorization: authorization,
        },
      },
    );

    if (response.status === 404) {
      return null;
    }

    if (!response.ok) {
      throw new Error(
        `Firestore getDocument failed (status=${response.status}, path=${path})`,
      );
    }

    const body: unknown = await response.json();
    return toDocument(body);
  };

  const runQuery: FirestoreRestClient["runQuery"] = async (input) => {
    const authorization = await resolveAuthorization();
    const body = buildStructuredQuery(input);

    const response = await fetch(
      buildRunQueryUrl(baseUrl, config.projectId, databaseId),
      {
        method: "POST",
        headers: {
          Authorization: authorization,
          "Content-Type": "application/json",
        },
        body: JSON.stringify(body),
      },
    );

    if (!response.ok) {
      throw new Error(
        `Firestore runQuery failed (status=${response.status}, collection=${input.collectionId})`,
      );
    }

    const parsed: unknown = await response.json();
    if (!Array.isArray(parsed)) {
      return [];
    }

    const documents: Record<string, JsonValue>[] = [];
    for (const entry of parsed) {
      if (!isRunQueryEntry(entry)) {
        continue;
      }
      if (entry.document === undefined) {
        continue;
      }
      const converted = toDocument(entry.document);
      if (converted !== null) {
        documents.push(converted);
      }
    }
    return documents;
  };

  return {
    getDocument,
    runQuery,
  };
};
