import { defineConfig } from "vitest/config";
import react from "@vitejs/plugin-react";
import tsconfigPaths from "vite-tsconfig-paths";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { storybookTest } from "@storybook/addon-vitest/vitest-plugin";
import { playwright } from "@vitest/browser-playwright";
import { reactAlias } from "./vitest.shared.mts";

const configDirectory = path.dirname(fileURLToPath(import.meta.url));

export default defineConfig({
  plugins: [tsconfigPaths(), react()],
  resolve: reactAlias,
  test: {
    include: [
      "**/tests/**/*.test.{ts,tsx}",
      "**/src/**/*.test.{ts,tsx}",
    ],
    exclude: ["**/node_modules/**"],
    environment: "jsdom",
    globals: true,
    projects: [
      {
        extends: true,
        test: {
          name: "unit",
          include: [
            "**/tests/**/*.test.{ts,tsx}",
            "**/src/**/*.test.{ts,tsx}",
          ],
          exclude: [
            "**/stories/**",
            "**/node_modules/**",
            "**/features/**",
            "**/infrastructures/**",
            "admin/tests/actions/!(analytics)**",
            "admin/tests/aspects/auth/**",
            "admin/tests/components/global/**",
            "admin/tests/infrastructure/**",
          ],
          environment: "jsdom",
          setupFiles: ["shared/tests/setup.ts"],
        },
      },
      {
        extends: true,
        plugins: [
          storybookTest({
            configDir: path.join(configDirectory, ".storybook"),
          }),
        ],
        test: {
          name: "storybook",
          browser: {
            enabled: true,
            headless: true,
            provider: playwright({}),
            instances: [
              {
                browser: "chromium",
              },
            ],
          },
          setupFiles: [".storybook/vitest.setup.ts"],
        },
      },
    ],
  },
});
