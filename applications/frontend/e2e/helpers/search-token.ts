const FIRESTORE_EMULATOR_HOST = "http://localhost:8085";
const PROJECT_IDENTIFIER = "demo-hut";
const DATABASE_IDENTIFIER = "(default)";
const BASE_URL = `${FIRESTORE_EMULATOR_HOST}/v1/projects/${PROJECT_IDENTIFIER}/databases/${DATABASE_IDENTIFIER}/documents`;

type ContentType = "article" | "memo" | "series";

type FirestoreValue = {
  stringValue?: string;
  integerValue?: string;
  doubleValue?: number;
  booleanValue?: boolean;
  timestampValue?: string;
  arrayValue?: {
    values?: Array<FirestoreValue>;
  };
  mapValue?: {
    fields?: Record<string, FirestoreValue>;
  };
  nullValue?: null;
};

type FirestoreDocument = {
  name?: string;
  fields?: Record<string, FirestoreValue>;
  createTime?: string;
  updateTime?: string;
};

type FirestoreRunQueryResponse = Array<{
  document?: FirestoreDocument;
  readTime?: string;
}>;

type ContentTokenIndex = {
  tokens: Array<string>;
};

const findDocumentBySlug = async (
  collection: string,
  slug: string,
): Promise<FirestoreDocument | undefined> => {
  const parentPath = `projects/${PROJECT_IDENTIFIER}/databases/${DATABASE_IDENTIFIER}/documents`;
  const url = `${FIRESTORE_EMULATOR_HOST}/v1/${parentPath}:runQuery`;

  const body = {
    structuredQuery: {
      from: [{ collectionId: collection }],
      where: {
        fieldFilter: {
          field: { fieldPath: "slug" },
          op: "EQUAL",
          value: { stringValue: slug },
        },
      },
      limit: 1,
    },
  };

  const response = await fetch(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });

  if (!response.ok) {
    return undefined;
  }

  const results: FirestoreRunQueryResponse = await response.json();

  if (results.length === 0 || results[0].document === undefined) {
    return undefined;
  }

  return results[0].document;
};

const extractIdentifier = (
  document: FirestoreDocument,
): string | undefined => {
  return document.fields?.identifier?.stringValue;
};

export const getArticleIdentifierBySlug = async (
  slug: string,
): Promise<string | undefined> => {
  const document = await findDocumentBySlug("articles", slug);

  if (document === undefined) {
    return undefined;
  }

  return extractIdentifier(document);
};

export const getMemoIdentifierBySlug = async (
  slug: string,
): Promise<string | undefined> => {
  const document = await findDocumentBySlug("memos", slug);

  if (document === undefined) {
    return undefined;
  }

  return extractIdentifier(document);
};

export const getSeriesIdentifierBySlug = async (
  slug: string,
): Promise<string | undefined> => {
  const document = await findDocumentBySlug("series", slug);

  if (document === undefined) {
    return undefined;
  }

  return extractIdentifier(document);
};

export const getContentTokenIndex = async (
  contentType: ContentType,
  contentIdentifier: string,
): Promise<ContentTokenIndex | undefined> => {
  const documentKey = `${contentType}:${contentIdentifier}`;
  const url = `${BASE_URL}/content-token-index/${documentKey}`;

  const response = await fetch(url);

  if (!response.ok) {
    return undefined;
  }

  const document: FirestoreDocument = await response.json();

  if (document.fields?.tokens?.arrayValue?.values === undefined) {
    return undefined;
  }

  const tokens = document.fields.tokens.arrayValue.values
    .map((value) => value.stringValue)
    .filter((value): value is string => value !== undefined);

  return { tokens };
};

export const waitForSearchTokens = async (
  contentType: ContentType,
  contentIdentifier: string,
  maxWaitMilliseconds: number = 30000,
): Promise<ContentTokenIndex | undefined> => {
  const pollingIntervalMilliseconds = 1000;
  const startTime = Date.now();

  while (Date.now() - startTime < maxWaitMilliseconds) {
    const index = await getContentTokenIndex(contentType, contentIdentifier);

    if (index !== undefined && index.tokens.length > 0) {
      return index;
    }

    await new Promise((resolve) =>
      setTimeout(resolve, pollingIntervalMilliseconds),
    );
  }

  return undefined;
};

export const verifySearchTokensDeleted = async (
  contentType: ContentType,
  contentIdentifier: string,
  maxWaitMilliseconds: number = 30000,
): Promise<boolean> => {
  const pollingIntervalMilliseconds = 1000;
  const startTime = Date.now();

  while (Date.now() - startTime < maxWaitMilliseconds) {
    const index = await getContentTokenIndex(contentType, contentIdentifier);

    if (index === undefined) {
      return true;
    }

    await new Promise((resolve) =>
      setTimeout(resolve, pollingIntervalMilliseconds),
    );
  }

  return false;
};

export const verifyTokenReference = async (
  tokenIdentifier: string,
  contentType: ContentType,
  contentIdentifier: string,
): Promise<boolean> => {
  const referenceKey = `${contentType}:${contentIdentifier}`;
  const url = `${BASE_URL}/search-tokens/${tokenIdentifier}/refs/${referenceKey}`;

  const response = await fetch(url);

  return response.ok;
};
