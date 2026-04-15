import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

describe("root package.json browserslist", () => {
  const rootPackageJsonPath = join(__dirname, "../../../../package.json");
  const rootPackageJson = JSON.parse(
    readFileSync(rootPackageJsonPath, "utf-8")
  ) as {
    browserslist?: string[];
  };

  it("browserslist フィールドが存在すること", () => {
    expect(rootPackageJson.browserslist).toBeDefined();
    expect(Array.isArray(rootPackageJson.browserslist)).toBe(true);
  });

  it("not dead が含まれること", () => {
    expect(rootPackageJson.browserslist).toContain("not dead");
  });

  it(">0.3% が含まれること", () => {
    expect(rootPackageJson.browserslist).toContain(">0.3%");
  });
});
