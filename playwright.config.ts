import { readFileSync, existsSync } from "node:fs";
import { resolve } from "node:path";
import { defineConfig, devices } from "@playwright/test";

type EnvEntry = {
  readonly key: string;
  readonly value: string;
};

const projectRoot = process.cwd();

/**
 * Normalize a raw .env value by trimming and unquoting.
 */
const normalizeEnvValue = (raw: string): string => {
  const trimmed = raw.trim();

  if (trimmed.length === 0) {
    return "";
  }

  const quote = trimmed[0];

  if (
    (quote === "'" || quote === '"' || quote === "`") &&
    trimmed.endsWith(quote)
  ) {
    return trimmed.slice(1, -1);
  }

  return trimmed;
};

/**
 * Parse a single .env line into a key/value pair.
 */
const parseEnvLine = (line: string): EnvEntry | null => {
  const trimmed = line.trim();

  if (trimmed.length === 0 || trimmed.startsWith("#")) {
    return null;
  }

  const withoutExport = trimmed.startsWith("export ")
    ? trimmed.slice(7).trim()
    : trimmed;
  const separatorIndex = withoutExport.indexOf("=");

  if (separatorIndex <= 0) {
    return null;
  }

  const key = withoutExport.slice(0, separatorIndex).trim();

  if (key.length === 0) {
    return null;
  }

  const rawValue = withoutExport.slice(separatorIndex + 1);
  const value = normalizeEnvValue(rawValue);

  return { key, value };
};

const lockedEnvKeys = new Set(Object.keys(process.env));
const mutableEnvKeys = new Set<string>();

/**
 * Load environment variables from a .env file if present.
 */
const loadEnvFile = (filePath: string): void => {
  if (!existsSync(filePath)) {
    return;
  }

  const content = readFileSync(filePath, "utf8");

  for (const line of content.split(/\r?\n/)) {
    const entry = parseEnvLine(line);

    if (!entry) {
      continue;
    }

    if (lockedEnvKeys.has(entry.key)) {
      continue;
    }

    const alreadySet = process.env[entry.key] !== undefined;

    if (alreadySet && !mutableEnvKeys.has(entry.key)) {
      continue;
    }

    process.env[entry.key] = entry.value;
    mutableEnvKeys.add(entry.key);
  }
};

const rootEnvPaths = [
  resolve(projectRoot, ".env"),
  resolve(projectRoot, ".env.local"),
];

for (const path of rootEnvPaths) {
  loadEnvFile(path);
}

const requiredE2EEnvKeys = [
  "E2E_AUTH_SECRET",
  "E2E_AUTH_UID",
  "E2E_AUTH_EMAIL",
] as const;

const hasMissingE2EEnv = requiredE2EEnvKeys.some((key) => {
  const value = process.env[key];
  return typeof value !== "string" || value.trim().length === 0;
});

if (hasMissingE2EEnv) {
  const adminEnvPaths = [
    resolve(projectRoot, "packages/admin/.env"),
    resolve(projectRoot, "packages/admin/.env.local"),
  ];

  for (const path of adminEnvPaths) {
    loadEnvFile(path);
  }
}

const baseURL = process.env.ADMIN_BASE_URL ?? "http://localhost:3001";

export default defineConfig({
  testDir: "./e2e",
  fullyParallel: true,
  forbidOnly: Boolean(process.env.CI),
  retries: process.env.CI ? 2 : 0,
  reporter: "list",
  globalSetup: "./playwright/global-setup.ts",
  use: {
    baseURL,
    trace: "on-first-retry",
    storageState: "playwright/.auth/admin.json",
  },
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
  ],
});
