import { defineConfig } from "vitest/config";
import tsconfigPaths from "vite-tsconfig-paths";
import path from "node:path";
import { fileURLToPath } from "node:url";

const dirname =
  typeof __dirname !== "undefined"
    ? __dirname
    : path.dirname(fileURLToPath(import.meta.url));

export default defineConfig({
  plugins: [tsconfigPaths()],
  resolve: {
    alias: {
      react: path.resolve(dirname, "packages/shared/node_modules/react"),
      "react-dom": path.resolve(
        dirname,
        "packages/shared/node_modules/react-dom",
      ),
    },
  },
  test: {
    include: [
      "packages/**/tests/infrastructures/**/*.test.{ts,tsx}",
    ],
    exclude: ["**/node_modules/**"],
    environment: "jsdom",
    globals: true,
    setupFiles: ["packages/shared/tests/setup.ts"],
  },
});
