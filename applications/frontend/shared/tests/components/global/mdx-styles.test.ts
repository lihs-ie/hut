import { describe, it, expect } from "vitest";
import { readFileSync } from "fs";
import { resolve } from "path";

const cssPath = resolve(
  __dirname,
  "../../../src/components/global/mdx.module.css",
);
const css = readFileSync(cssPath, "utf-8");

describe("components/global/mdx.module.css", () => {
  it(".header が flexbox レイアウトを持つ", () => {
    expect(css).toContain(".header");
    expect(css).toContain("display: flex");
  });

  it(".header が justify-content: space-between を持つ", () => {
    const headerStart = css.indexOf(".header");
    const headerSection = css.slice(
      headerStart,
      css.indexOf("}", headerStart) + 1,
    );
    expect(headerSection).toContain("justify-content: space-between");
  });

  it(".filename クラスが定義されている", () => {
    expect(css).toContain(".filename");
  });

  it(".meta クラスが定義されている", () => {
    expect(css).toContain(".meta");
  });

  it(".copy クラスが定義されている", () => {
    expect(css).toContain(".copy");
  });

  it(".pre が padding を持つ", () => {
    const preStart = css.indexOf(".pre");
    const preSection = css.slice(preStart, css.indexOf("}", preStart) + 1);
    expect(preSection).toMatch(/padding/);
  });
});
