import { describe, it, expect, vi } from "vitest";
import type { Root, Code } from "mdast";

const mockRender = vi.fn(async (diagrams: string[]) =>
  diagrams.map((diagram, index) => {
    if (diagram.includes("invalid syntax @@@@")) {
      return {
        status: "rejected" as const,
        reason: new Error("Parse error"),
      };
    }
    return {
      status: "fulfilled" as const,
      value: {
        id: `mermaid-${index}`,
        diagram,
        svg: `<svg xmlns="http://www.w3.org/2000/svg" id="mermaid-${index}"><g>rendered</g></svg>`,
        width: 200,
        height: 100,
      },
    };
  }),
);

vi.mock("mermaid-isomorphic", () => ({
  createMermaidRenderer: vi.fn(() => mockRender),
}));

const mockSanitize = vi.fn((svg: string) => svg);

vi.mock("@shared/components/molecules/mermaid/sanitize", () => ({
  sanitizeMermaidSvg: mockSanitize,
}));

vi.mock("../components/molecules/mermaid/sanitize", () => ({
  sanitizeMermaidSvg: mockSanitize,
}));

type Transformer = (tree: Root) => Promise<void> | void;
type PluginFactory = () => Transformer | undefined;

const applyPlugin = async (tree: Root): Promise<void> => {
  const module = await import("@shared/plugins/remark-mermaid");
  const factory = module.remarkMermaid as unknown as PluginFactory;
  const transform = factory();
  if (typeof transform === "function") {
    const result = transform(tree);
    if (result instanceof Promise) {
      await result;
    }
  }
};

describe("remarkMermaid", () => {
  describe("mermaidコードブロックの変換", () => {
    it("mermaidコードブロックをHTMLノードに変換する", async () => {
      const tree: Root = {
        type: "root",
        children: [
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "flowchart TD\n  A --> B",
          } as Code,
        ],
      };

      await applyPlugin(tree);

      expect(tree.children[0].type).toBe("html");
      const htmlNode = tree.children[0] as { type: string; value: string };
      expect(htmlNode.value).toContain("<svg");
    });

    it("mermaid以外のコードブロックは変換しない", async () => {
      const tree: Root = {
        type: "root",
        children: [
          {
            type: "code",
            lang: "typescript",
            meta: null,
            value: "console.log('hello')",
          } as Code,
        ],
      };

      await applyPlugin(tree);

      expect(tree.children[0].type).toBe("code");
    });

    it("不正なmermaid構文でフォールバック表示になる", async () => {
      const tree: Root = {
        type: "root",
        children: [
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "invalid syntax @@@@",
          } as Code,
        ],
      };

      await applyPlugin(tree);

      expect(tree.children[0].type).toBe("html");
      const htmlNode = tree.children[0] as { type: string; value: string };
      expect(htmlNode.value).toContain("invalid syntax @@@@");
    });

    it("複数のmermaidブロックをそれぞれ変換する", async () => {
      const tree: Root = {
        type: "root",
        children: [
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "flowchart TD\n  A --> B",
          } as Code,
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "sequenceDiagram\n  A->>B: hello",
          } as Code,
        ],
      };

      await applyPlugin(tree);

      expect(tree.children[0].type).toBe("html");
      expect(tree.children[1].type).toBe("html");
      const first = tree.children[0] as { type: string; value: string };
      const second = tree.children[1] as { type: string; value: string };
      expect(first.value).toContain("<svg");
      expect(second.value).toContain("<svg");
    });

    it("生成されたHTMLにmermaid-svgクラスのラッパーdivが含まれる", async () => {
      const tree: Root = {
        type: "root",
        children: [
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "flowchart TD\n  A --> B",
          } as Code,
        ],
      };

      await applyPlugin(tree);

      const htmlNode = tree.children[0] as { type: string; value: string };
      expect(htmlNode.value).toContain("mermaid-svg");
    });

    it("sanitizeMermaidSvgを呼び出してSVGをサニタイズする", async () => {
      mockSanitize.mockClear();

      const tree: Root = {
        type: "root",
        children: [
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "flowchart TD\n  A --> B",
          } as Code,
        ],
      };

      await applyPlugin(tree);

      expect(mockSanitize).toHaveBeenCalled();
    });
  });
});
