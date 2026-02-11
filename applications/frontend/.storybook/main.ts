import type { StorybookConfig } from "@storybook/nextjs-vite";
import path from "node:path";
import { fileURLToPath } from "node:url";
import tsconfigPaths from "vite-tsconfig-paths";

const applyViteConfig: StorybookConfig["viteFinal"] = async (config) => {
  const configDir = path.dirname(fileURLToPath(import.meta.url));
  const workspaceRoot = path.resolve(configDir, "..");
  const projects = [
    path.resolve(workspaceRoot, "shared/tsconfig.json"),
    path.resolve(workspaceRoot, "admin/tsconfig.json"),
  ];

  const plugins = [...(config.plugins ?? []), tsconfigPaths({ projects })];

  return {
    ...config,
    plugins,
  };
};

const config: StorybookConfig = {
  stories: [
    "../shared/tests/**/*.@(mdx|stories.@(js|jsx|mjs|ts|tsx))",
    "../admin/tests/**/*.@(mdx|stories.@(js|jsx|mjs|ts|tsx))",
  ],
  addons: [
    "@chromatic-com/storybook",
    "@storybook/addon-vitest",
    "@storybook/addon-a11y",
    "@storybook/addon-docs",
    "@storybook/addon-onboarding",
  ],
  framework: "@storybook/nextjs-vite",
  staticDirs: ["../shared/public", "../admin/public"],
  features: {
    experimentalRSC: true,
  },
  viteFinal: applyViteConfig,
};

export default config;
