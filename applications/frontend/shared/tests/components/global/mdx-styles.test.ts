import { describe, it, expect } from "vitest";
import { readFileSync } from "fs";
import { resolve } from "path";

const cssPath = resolve(
  __dirname,
  "../../../src/components/global/mdx.module.css",
);
const css = readFileSync(cssPath, "utf-8");

describe("components/global/mdx.module.css", () => {
  it(".header クラスが定義されていない", () => {
    expect(css).not.toContain(".header");
  });

  it(".filename クラスが定義されていない", () => {
    expect(css).not.toContain(".filename");
  });

  it(".meta クラスが定義されていない", () => {
    expect(css).not.toContain(".meta");
  });

  it(".language クラスが定義されていない", () => {
    expect(css).not.toContain(".language");
  });

  it(".copy クラスが定義されている", () => {
    expect(css).toContain(".copy");
  });

  it(".copy がホバー時に表示される（opacity: 1）", () => {
    expect(css).toContain(".container:hover .copy");
    const hoverStart = css.indexOf(".container:hover .copy");
    const hoverSection = css.slice(hoverStart, css.indexOf("}", hoverStart) + 1);
    expect(hoverSection).toContain("opacity: 1");
  });

  it(".copy がフォーカス時にも表示される（focus-within）", () => {
    expect(css).toContain(".container:focus-within .copy");
  });

  it(".copy の初期状態が非表示（opacity: 0）", () => {
    const copyStart = css.indexOf(".copy {");
    const copySection = css.slice(copyStart, css.indexOf("}", copyStart) + 1);
    expect(copySection).toContain("opacity: 0");
  });

  it(".pre が border-radius: 4px を持つ（全角丸め）", () => {
    const preStart = css.indexOf(".pre {");
    const preSection = css.slice(preStart, css.indexOf("}", preStart) + 1);
    expect(preSection).toContain("border-radius: 4px");
    expect(preSection).not.toContain("0 0 4px 4px");
  });

  it(".pre が padding を持つ", () => {
    const preStart = css.indexOf(".pre {");
    const preSection = css.slice(preStart, css.indexOf("}", preStart) + 1);
    expect(preSection).toMatch(/padding/);
  });
});
