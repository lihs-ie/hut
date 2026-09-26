/** @vitest-environment node */
import { describe, expect, it } from "vitest";
import { sanitizeMermaidSvg } from "@shared/components/molecules/mermaid/sanitize";

describe("Mermaid sanitizer SSR import", () => {
  it("DOM がない環境でもモジュールを読み込める", () => {
    expect(sanitizeMermaidSvg).toBeTypeOf("function");
  });
});
