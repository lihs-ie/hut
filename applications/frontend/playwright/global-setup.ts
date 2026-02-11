import { mkdir } from "node:fs/promises";
import { dirname } from "node:path";
import {
  request as playwrightRequest,
  type FullConfig,
} from "@playwright/test";

type E2EAuthConfig = {
  readonly baseURL: string;
  readonly secret: string;
  readonly uid: string;
  readonly email: string;
  readonly storageStatePath: string;
};

const storageStatePath = "playwright/.auth/admin.json";
const authEndpoint = "/api/e2e/auth";

/**
 * Resolve the base URL for Playwright.
 */
const resolveBaseUrl = (config: FullConfig): string => {
  const fromEnv = process.env.ADMIN_BASE_URL?.trim();

  if (fromEnv) {
    return fromEnv;
  }

  const projectBaseUrl = config.projects[0]?.use?.baseURL;

  if (typeof projectBaseUrl === "string" && projectBaseUrl.trim().length > 0) {
    return projectBaseUrl;
  }

  return "http://localhost:3001";
};

/**
 * Resolve E2E auth configuration from the environment.
 */
const resolveAuthConfig = (baseURL: string): E2EAuthConfig => {
  const secret = process.env.E2E_AUTH_SECRET?.trim();
  const uid = process.env.E2E_AUTH_UID?.trim();
  const email = process.env.E2E_AUTH_EMAIL?.trim();

  if (!secret || !uid || !email) {
    throw new Error(
      "E2E auth requires E2E_AUTH_SECRET, E2E_AUTH_UID, and E2E_AUTH_EMAIL",
    );
  }

  return {
    baseURL,
    secret,
    uid,
    email,
    storageStatePath,
  };
};

/**
 * Ensure the storage state directory exists.
 */
const ensureStorageStateDir = async (path: string): Promise<void> => {
  await mkdir(dirname(path), { recursive: true });
};

/**
 * Create storage state by calling the E2E auth endpoint.
 */
const createStorageState = async (config: E2EAuthConfig): Promise<void> => {
  const requestContext = await playwrightRequest.newContext({
    baseURL: config.baseURL,
  });

  const response = await requestContext.post(authEndpoint, {
    data: { uid: config.uid, email: config.email },
    headers: {
      "x-e2e-auth-secret": config.secret,
    },
  });

  if (!response.ok()) {
    const body = await response.text();
    await requestContext.dispose();
    throw new Error(
      `E2E auth failed: ${response.status()} ${response.statusText()} ${body}`,
    );
  }

  await ensureStorageStateDir(config.storageStatePath);
  await requestContext.storageState({ path: config.storageStatePath });
  await requestContext.dispose();
};

/**
 * Global setup to authenticate once and persist storage state.
 */
const globalSetup = async (config: FullConfig): Promise<void> => {
  const baseURL = resolveBaseUrl(config);
  const authConfig = resolveAuthConfig(baseURL);

  await createStorageState(authConfig);
};

export default globalSetup;
