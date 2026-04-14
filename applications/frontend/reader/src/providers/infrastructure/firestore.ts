import {
  createAccessTokenProvider,
  createFirestoreRestArticleRepository,
  createFirestoreRestChapterRepository,
  createFirestoreRestClient,
  createFirestoreRestMemoRepository,
  createFirestoreRestSeriesRepository,
  type AccessTokenProvider,
  type FirestoreRestClient,
} from "@/infrastructures/firestore-rest";
import type { ArticleRepository } from "@shared/domains/articles";
import type { ChapterRepository } from "@shared/domains/series/chapter";
import type { MemoRepository } from "@shared/domains/memo";
import type { SeriesRepository } from "@shared/domains/series";

const DEFAULT_EMULATOR_HOST = "127.0.0.1:8085";

const readRequiredEnvironmentVariable = (name: string): string => {
  const value = process.env[name];
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`Environment variable ${name} is required.`);
  }
  return value;
};

const isEmulatorEnabled = (): boolean =>
  process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true";

const resolveEmulatorBaseUrl = (): string => {
  const host = process.env.FIRESTORE_EMULATOR_HOST ?? DEFAULT_EMULATOR_HOST;
  return `http://${host}/v1`;
};

const readProjectId = (): string => {
  const explicit = process.env.FIREBASE_PROJECT_ID;
  if (typeof explicit === "string" && explicit.length > 0) {
    return explicit;
  }
  if (isEmulatorEnabled()) {
    const emulatorProjectId = process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID;
    if (typeof emulatorProjectId === "string" && emulatorProjectId.length > 0) {
      return emulatorProjectId;
    }
    return "demo-hut";
  }
  return readRequiredEnvironmentVariable("NEXT_PUBLIC_FIREBASE_PROJECT_ID");
};

const normalizePrivateKey = (value: string): string =>
  value.includes("\\n") ? value.replace(/\\n/g, "\n") : value;

let cachedAccessTokenProvider: AccessTokenProvider | null = null;

const getAccessTokenProvider = (): AccessTokenProvider => {
  if (cachedAccessTokenProvider !== null) {
    return cachedAccessTokenProvider;
  }

  const clientEmail = readRequiredEnvironmentVariable(
    "FIREBASE_SERVICE_ACCOUNT_EMAIL",
  );
  const privateKey = normalizePrivateKey(
    readRequiredEnvironmentVariable("FIREBASE_SERVICE_ACCOUNT_PRIVATE_KEY"),
  );

  cachedAccessTokenProvider = createAccessTokenProvider({
    clientEmail,
    privateKey,
  });
  return cachedAccessTokenProvider;
};

let cachedClient: FirestoreRestClient | null = null;

const getClient = (): FirestoreRestClient => {
  if (cachedClient !== null) {
    return cachedClient;
  }

  if (isEmulatorEnabled()) {
    cachedClient = createFirestoreRestClient({
      projectId: readProjectId(),
      baseUrl: resolveEmulatorBaseUrl(),
    });
    return cachedClient;
  }

  cachedClient = createFirestoreRestClient({
    projectId: readProjectId(),
    accessTokenProvider: getAccessTokenProvider(),
  });
  return cachedClient;
};

let cachedArticleRepository: ArticleRepository | null = null;
let cachedChapterRepository: ChapterRepository | null = null;
let cachedMemoRepository: MemoRepository | null = null;
let cachedSeriesRepository: SeriesRepository | null = null;

export const ReaderFirestoreProvider = {
  get articleRepository(): ArticleRepository {
    if (cachedArticleRepository === null) {
      cachedArticleRepository = createFirestoreRestArticleRepository(getClient());
    }
    return cachedArticleRepository;
  },
  get chapterRepository(): ChapterRepository {
    if (cachedChapterRepository === null) {
      cachedChapterRepository = createFirestoreRestChapterRepository(getClient());
    }
    return cachedChapterRepository;
  },
  get memoRepository(): MemoRepository {
    if (cachedMemoRepository === null) {
      cachedMemoRepository = createFirestoreRestMemoRepository(getClient());
    }
    return cachedMemoRepository;
  },
  get seriesRepository(): SeriesRepository {
    if (cachedSeriesRepository === null) {
      cachedSeriesRepository = createFirestoreRestSeriesRepository(getClient());
    }
    return cachedSeriesRepository;
  },
} as const;
