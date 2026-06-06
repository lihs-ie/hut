import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

describe("@hut/shared package.json exports", () => {
  const packageJsonPath = join(__dirname, "../package.json");
  const packageJson = JSON.parse(readFileSync(packageJsonPath, "utf-8")) as {
    exports?: Record<string, string>;
  };

  const exports = packageJson.exports ?? {};

  it("./global.css が exports に含まれること", () => {
    expect(exports).toHaveProperty("./global.css");
  });

  it("./components/* が exports に含まれること", () => {
    expect(exports).toHaveProperty("./components/*");
  });

  it("./actions/* が exports に含まれること", () => {
    expect(exports).toHaveProperty("./actions/*");
  });

  it("./domains/* が exports に含まれること", () => {
    expect(exports).toHaveProperty("./domains/*");
  });
});
