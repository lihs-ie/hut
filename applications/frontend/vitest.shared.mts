import path from "node:path";
import { fileURLToPath } from "node:url";
import type { UserConfig } from "vitest/config";

const currentDirectory =
  typeof __dirname !== "undefined"
    ? __dirname
    : path.dirname(fileURLToPath(import.meta.url));

export const reactAlias: UserConfig["resolve"] = {
  alias: {
    react: path.resolve(currentDirectory, "shared/node_modules/react"),
    "react-dom": path.resolve(
      currentDirectory,
      "shared/node_modules/react-dom",
    ),
  },
};
