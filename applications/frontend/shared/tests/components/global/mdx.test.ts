import { describe, it, expect } from "vitest";
import { mdxOptions } from "@shared/components/global/mdx";
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
});
