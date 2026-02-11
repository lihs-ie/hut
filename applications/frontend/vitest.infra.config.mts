import { defineConfig } from "vitest/config";
import tsconfigPaths from "vite-tsconfig-paths";
import { reactAlias } from "./vitest.shared.mts";

export default defineConfig({
  plugins: [tsconfigPaths()],
  resolve: reactAlias,
  test: {
    include: ["**/tests/infrastructures/**/*.test.{ts,tsx}"],
    exclude: ["**/node_modules/**"],
    environment: "jsdom",
    globals: true,
    setupFiles: ["shared/tests/setup.ts"],
  },
});
