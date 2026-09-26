import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

describe("global.css", () => {
  const cssPath = join(
    __dirname,
    "../src/global.css"
  );
  const cssContent = readFileSync(cssPath, "utf-8");

  it("@custom-variant 宣言が含まれないこと", () => {
    expect(cssContent).not.toContain("@custom-variant");
  });

  it(".dark セレクタが存在すること", () => {
    expect(cssContent).toContain(".dark");
  });
});
