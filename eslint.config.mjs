// For more info, see https://github.com/storybookjs/eslint-plugin-storybook#configuration-flat-config-format
import storybook from "eslint-plugin-storybook";

import { defineConfig, globalIgnores } from "eslint/config";
import nextVitals from "eslint-config-next/core-web-vitals";
import nextTs from "eslint-config-next/typescript";

const noUnusedVarsRule = {
  "@typescript-eslint/no-unused-vars": [
    "error",
    {
      argsIgnorePattern: "^_",
      varsIgnorePattern: "^_",
      caughtErrorsIgnorePattern: "^_",
    },
  ],
};

const eslintConfig = defineConfig([
  globalIgnores([
    "**/node_modules/**",
    "**/.next/**",
    "**/out/**",
    "**/build/**",
    "**/storybook-static/**",
    "**/next-env.d.ts",
  ]),
  {
    name: "next/admin",
    files: ["packages/admin/**/*.{js,jsx,ts,tsx}"],
    extends: [...nextVitals, ...nextTs],
    settings: {
      next: {
        rootDir: "packages/admin",
      },
    },
    rules: noUnusedVarsRule,
  },
  {
    name: "next/reader",
    files: ["packages/reader/**/*.{js,jsx,ts,tsx}"],
    extends: [...nextVitals, ...nextTs],
    settings: {
      next: {
        rootDir: "packages/reader",
      },
    },
    rules: noUnusedVarsRule,
  },
  {
    name: "next/shared",
    files: ["packages/shared/src/**/*.{js,jsx,ts,tsx}"],
    extends: [...nextVitals, ...nextTs],
    settings: {
      next: {
        rootDir: ["packages/admin", "packages/reader"],
      },
    },
    rules: noUnusedVarsRule,
  },
  {
    name: "storybook",
    files: ["packages/shared/tests/stories/**/*.stories.{js,jsx,ts,tsx}"],
    extends: [...nextTs, ...storybook.configs["flat/recommended"]],
    rules: noUnusedVarsRule,
  },
]);

export default eslintConfig;
