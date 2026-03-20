import { describe, it, expect } from "vitest";
import { mdxOptions, extractFilenameFromMeta } from "@shared/components/global/mdx";
import remarkBreaks from "remark-breaks";

describe("components/global/mdx", () => {
  describe("mdxOptions", () => {
    describe("remarkPlugins", () => {
      it("remark-breaks が含まれている", () => {
        expect(mdxOptions).toBeDefined();
        const innerOptions = mdxOptions!.mdxOptions;
        const plugins = innerOptions?.remarkPlugins ?? [];
        const pluginFunctions = plugins.map((plugin) =>
          Array.isArray(plugin) ? plugin[0] : plugin,
        );
        expect(pluginFunctions).toContain(remarkBreaks);
      });
    });
  });

  describe("extractFilenameFromMeta", () => {
    it(":filename.ts 形式からファイル名を抽出する", () => {
      expect(extractFilenameFromMeta(":example.ts")).toBe("example.ts");
    });

    it(":path/to/file.tsx 形式からファイル名を抽出する", () => {
      expect(extractFilenameFromMeta(":path/to/file.tsx")).toBe("path/to/file.tsx");
    });

    it("コロンが含まれていない場合は null を返す", () => {
      expect(extractFilenameFromMeta("")).toBeNull();
    });

    it("コロンで始まらない場合は null を返す", () => {
      expect(extractFilenameFromMeta("someother meta")).toBeNull();
    });

    it("コロンの後にファイル名がない場合は null を返す", () => {
      expect(extractFilenameFromMeta(":")).toBeNull();
    });
  });
});
