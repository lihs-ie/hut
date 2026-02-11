import storybook from "eslint-plugin-storybook";

import { defineConfig, globalIgnores } from "eslint/config";
import nextVitals from "eslint-config-next/core-web-vitals";
import nextTs from "eslint-config-next/typescript";

const unusedVarsRules = {
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
    files: ["admin/**/*.{js,jsx,ts,tsx}"],
    extends: [...nextVitals, ...nextTs],
    settings: { next: { rootDir: "admin" } },
    rules: unusedVarsRules,
  },
  {
    name: "next/reader",
    files: ["reader/**/*.{js,jsx,ts,tsx}"],
    extends: [...nextVitals, ...nextTs],
    settings: { next: { rootDir: "reader" } },
    rules: unusedVarsRules,
  },
  {
    name: "next/shared",
    files: ["shared/src/**/*.{js,jsx,ts,tsx}"],
    extends: [...nextVitals, ...nextTs],
    settings: { next: { rootDir: ["admin", "reader"] } },
    rules: unusedVarsRules,
  },
  {
    name: "shared/tests",
    files: ["shared/tests/**/*.{js,jsx,ts,tsx}"],
    ignores: ["shared/tests/stories/**/*.stories.{js,jsx,ts,tsx}"],
    extends: [...nextTs],
    rules: unusedVarsRules,
  },
  {
    name: "storybook",
    files: ["shared/tests/stories/**/*.stories.{js,jsx,ts,tsx}"],
    extends: [...nextTs, ...storybook.configs["flat/recommended"]],
    rules: unusedVarsRules,
  },
  {
    name: "e2e",
    files: ["e2e/**/*.{js,jsx,ts,tsx}"],
    extends: [...nextTs],
    rules: unusedVarsRules,
  },
]);

export default eslintConfig;
