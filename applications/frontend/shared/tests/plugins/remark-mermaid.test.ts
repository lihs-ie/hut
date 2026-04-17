import { describe, it, expect, vi } from "vitest";
import type { Root, RootContent } from "mdast";
import { unified, type Plugin } from "unified";

const assertHtmlNode = (
  node: RootContent,
): { type: "html"; value: string } => {
  if (node.type !== "html") {
    throw new Error(`expected html node but got ${node.type}`);
  }
  return node;
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
    it("mermaidコードブロックをHTMLノードに変換する", async () => {
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

      const htmlNode = assertHtmlNode(tree.children[0]);
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
          },
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
          },
        ],
      };

      await applyPlugin(tree);

      const htmlNode = assertHtmlNode(tree.children[0]);
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

      const first = assertHtmlNode(tree.children[0]);
      const second = assertHtmlNode(tree.children[1]);
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
          },
        ],
      };

      await applyPlugin(tree);

      const htmlNode = assertHtmlNode(tree.children[0]);
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
          },
        ],
      };

      await applyPlugin(tree);

      expect(mockSanitize).toHaveBeenCalled();
    });

    it("フォールバック時に特殊文字をHTMLエスケープする", async () => {
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

      const htmlNode = assertHtmlNode(tree.children[0]);
      expect(htmlNode.value).not.toContain("<script>");
      expect(htmlNode.value).toContain("&lt;script&gt;");
      expect(htmlNode.value).toContain("&#039;xss&#039;");
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
