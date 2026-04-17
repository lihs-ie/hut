import { describe, it, expect, vi } from "vitest";
import type { Root, RootContent } from "mdast";
import type { MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { unified, type Plugin } from "unified";

const assertMermaidJsxNode = (node: RootContent): MdxJsxFlowElement => {
  if (node.type !== "mdxJsxFlowElement") {
    throw new Error(`expected mdxJsxFlowElement node but got ${node.type}`);
  }
  if (node.name !== "MermaidSvg") {
    throw new Error(`expected MermaidSvg element but got ${node.name}`);
  }
  return node;
};

const getAttribute = (node: MdxJsxFlowElement, name: string): string => {
  const attribute = node.attributes.find(
    (item) => item.type === "mdxJsxAttribute" && item.name === name,
  );
  if (!attribute || attribute.type !== "mdxJsxAttribute") {
    throw new Error(`attribute ${name} not found`);
  }
  if (typeof attribute.value !== "string") {
    throw new Error(`attribute ${name} must be string`);
  }
  return attribute.value;
};

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

const mockCreateRenderer = vi.fn(() => mockRender);

vi.mock("mermaid-isomorphic", () => ({
  createMermaidRenderer: mockCreateRenderer,
}));

const mockSanitize = vi.fn((svg: string) => svg);

vi.mock("@shared/components/molecules/mermaid/sanitize", () => ({
  sanitizeMermaidSvg: mockSanitize,
}));

const applyPlugin = async (tree: Root): Promise<void> => {
  const module = await import("@shared/plugins/remark-mermaid");
  const plugin: Plugin<[], Root> = module.remarkMermaid;
  const processor = unified().use(plugin);
  await processor.run(tree);
};

describe("remarkMermaid", () => {
  describe("mermaidコードブロックの変換", () => {
    it("mermaidコードブロックをMermaidSvg JSXノードに変換する", async () => {
      const tree: Root = {
        type: "root",
        children: [
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "flowchart TD\n  A --> B",
          },
        ],
      };

      await applyPlugin(tree);

      const node = assertMermaidJsxNode(tree.children[0]);
      expect(getAttribute(node, "html")).toContain("<svg");
      expect(getAttribute(node, "fallback")).toBe("false");
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
          },
        ],
      };

      await applyPlugin(tree);

      expect(tree.children[0].type).toBe("code");
    });

    it("不正なmermaid構文でfallback属性がtrueになる", async () => {
      const tree: Root = {
        type: "root",
        children: [
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "invalid syntax @@@@",
          },
        ],
      };

      await applyPlugin(tree);

      const node = assertMermaidJsxNode(tree.children[0]);
      expect(getAttribute(node, "fallback")).toBe("true");
      expect(getAttribute(node, "html")).toContain("invalid syntax @@@@");
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
          },
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "sequenceDiagram\n  A->>B: hello",
          },
        ],
      };

      await applyPlugin(tree);

      const first = assertMermaidJsxNode(tree.children[0]);
      const second = assertMermaidJsxNode(tree.children[1]);
      expect(getAttribute(first, "html")).toContain("<svg");
      expect(getAttribute(second, "html")).toContain("<svg");
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
          },
        ],
      };

      await applyPlugin(tree);

      expect(mockSanitize).toHaveBeenCalled();
    });

    it("フォールバック時は元のコードをhtml属性に保持する", async () => {
      const tree: Root = {
        type: "root",
        children: [
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "invalid syntax @@@@ <script>alert('xss')</script>",
          },
        ],
      };

      await applyPlugin(tree);

      const node = assertMermaidJsxNode(tree.children[0]);
      expect(getAttribute(node, "fallback")).toBe("true");
      expect(getAttribute(node, "html")).toContain("<script>");
    });

    it("空のコードブロックツリーでもエラーを起こさない", async () => {
      const tree: Root = {
        type: "root",
        children: [],
      };

      await expect(applyPlugin(tree)).resolves.toBeUndefined();
      expect(tree.children).toHaveLength(0);
    });
  });

  describe("レンダラーのシングルトン化", () => {
    it("複数回の実行でcreateMermaidRendererは一度しか呼ばれない", async () => {
      const initialCallCount = mockCreateRenderer.mock.calls.length;

      const createTree = (): Root => ({
        type: "root",
        children: [
          {
            type: "code",
            lang: "mermaid",
            meta: null,
            value: "flowchart TD\n  A --> B",
          },
        ],
      });

      await applyPlugin(createTree());
      await applyPlugin(createTree());
      await applyPlugin(createTree());

      expect(
        mockCreateRenderer.mock.calls.length - initialCallCount,
      ).toBeLessThanOrEqual(1);
    });
  });
});
