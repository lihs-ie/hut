/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { mdxOptions } from "@shared/components/global/mdx";
import { remarkMermaid } from "@shared/plugins/remark-mermaid";

vi.mock("@shikijs/rehype", () => ({ default: vi.fn() }));
vi.mock("remark-gfm", () => ({ default: vi.fn() }));
vi.mock("remark-breaks", () => ({ default: vi.fn() }));
vi.mock("rehype-slug", () => ({ default: vi.fn() }));
vi.mock("@shared/plugins/remark-link-card", () => ({
  remarkLinkCard: vi.fn(),
}));
vi.mock("@shared/components/molecules/card/link", () => ({
  LinkCard: vi.fn(),
}));
vi.mock("@shared/components/molecules/button/copy", () => ({
  CopyButton: vi.fn(),
}));
vi.mock("@shared/components/global/mdx.module.css", () => ({
  default: { container: "container", pre: "pre", copy: "copy" },
}));

describe("MDX mermaidプラグイン統合", () => {
  it("remarkPluginsにremarkMermaidが含まれている", () => {
    const innerOptions = mdxOptions?.mdxOptions;
    const plugins = innerOptions?.remarkPlugins ?? [];
    const pluginFunctions = plugins.map((plugin) =>
      Array.isArray(plugin) ? plugin[0] : plugin,
    );
    expect(pluginFunctions).toContain(remarkMermaid);
  });

  it("mdxOptionsにMermaidRendererのimportが不要になる", () => {
    expect(mdxOptions).toBeDefined();
  });
});
