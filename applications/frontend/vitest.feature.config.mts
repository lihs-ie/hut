import { defineConfig } from "vitest/config";
import tsconfigPaths from "vite-tsconfig-paths";
import { reactAlias } from "./vitest.shared.mts";

export default defineConfig({
  plugins: [tsconfigPaths()],
  resolve: reactAlias,
  test: {
    include: ["shared/tests/features/**/*.test.ts"],
    exclude: ["**/node_modules/**"],
    environment: "node",
    globals: true,
    fileParallelism: false,
  },
});
