import { readFileSync, existsSync } from "node:fs";
import { resolve } from "node:path";
import { defineConfig, devices } from "@playwright/test";

const projectRoot = process.cwd();

const stripQuotes = (value: string): string => {
  const trimmed = value.trim();
  const quote = trimmed[0];

  if (
    trimmed.length >= 2 &&
    (quote === "'" || quote === '"' || quote === "`") &&
    trimmed.endsWith(quote)
  ) {
    return trimmed.slice(1, -1);
  }

  return trimmed;
};

const loadEnvFile = (filePath: string): void => {
  if (!existsSync(filePath)) {
    return;
  }

  const content = readFileSync(filePath, "utf8");

  for (const line of content.split(/\r?\n/)) {
    const trimmed = line.trim();

    if (trimmed.length === 0 || trimmed.startsWith("#")) {
      continue;
    }

    const withoutExport = trimmed.startsWith("export ")
      ? trimmed.slice(7).trim()
      : trimmed;

    const separatorIndex = withoutExport.indexOf("=");

    if (separatorIndex <= 0) {
      continue;
    }

    const key = withoutExport.slice(0, separatorIndex).trim();

    if (key.length === 0 || process.env[key] !== undefined) {
      continue;
    }

    process.env[key] = stripQuotes(withoutExport.slice(separatorIndex + 1));
  }
};

const envFiles = [
  resolve(projectRoot, ".env"),
  resolve(projectRoot, ".env.local"),
  resolve(projectRoot, "admin/.env"),
  resolve(projectRoot, "admin/.env.local"),
];

for (const envFile of envFiles) {
  loadEnvFile(envFile);
}

const baseURL = process.env.ADMIN_BASE_URL ?? "http://localhost:3001";

export default defineConfig({
  testDir: "./e2e",
  fullyParallel: true,
  forbidOnly: Boolean(process.env.CI),
  retries: process.env.CI ? 2 : 0,
  reporter: process.env.CI ? "blob" : "list",
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
