import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

describe("reader package.json", () => {
  const packageJsonPath = join(__dirname, "../package.json");
  const packageJson = JSON.parse(readFileSync(packageJsonPath, "utf-8")) as {
    dependencies?: Record<string, string>;
    devDependencies?: Record<string, string>;
  };

  const allDependencies = {
    ...packageJson.dependencies,
    ...packageJson.devDependencies,
  };

  it("rehype-highlight が含まれないこと", () => {
    expect(allDependencies).not.toHaveProperty("rehype-highlight");
  });

  it("rehype-pretty-code が含まれないこと", () => {
    expect(allDependencies).not.toHaveProperty("rehype-pretty-code");
  });

  it("@mdx-js/react が含まれないこと", () => {
    expect(allDependencies).not.toHaveProperty("@mdx-js/react");
  });
});
