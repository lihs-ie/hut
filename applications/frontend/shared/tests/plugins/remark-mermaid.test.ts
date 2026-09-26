import { describe, it, expect } from "vitest";
import type { Root, RootContent } from "mdast";
import type { MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { unified } from "unified";
import { remarkMermaid } from "@shared/plugins/remark-mermaid";

const assertMermaidJsxNode = (node: RootContent): MdxJsxFlowElement => {
  if (node.type !== "mdxJsxFlowElement") {
    throw new Error(`expected mdxJsxFlowElement node but got ${node.type}`);
  }
  if (node.name !== "MermaidClient") {
    throw new Error(`expected MermaidClient element but got ${node.name}`);
  }
  return node;
};

const getStringAttribute = (node: MdxJsxFlowElement, name: string): string => {
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

const applyPlugin = async (tree: Root): Promise<void> => {
  const processor = unified().use(remarkMermaid);
  await processor.run(tree);
};

describe("remarkMermaid", () => {
  it("mermaidコードブロックをMermaidClient JSXノードに変換する", async () => {
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
    expect(getStringAttribute(node, "code")).toBe("flowchart TD\n  A --> B");
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
    expect(getStringAttribute(first, "code")).toBe("flowchart TD\n  A --> B");
    expect(getStringAttribute(second, "code")).toBe(
      "sequenceDiagram\n  A->>B: hello",
    );
  });

  it("mermaidコードはそのままcode属性に渡される (サニタイズはClient側で実行)", async () => {
    const tree: Root = {
      type: "root",
      children: [
        {
          type: "code",
          lang: "mermaid",
          meta: null,
          value: "graph TD\n  A[<script>alert('xss')</script>]",
        },
      ],
    };

    await applyPlugin(tree);

    const node = assertMermaidJsxNode(tree.children[0]);
    expect(getStringAttribute(node, "code")).toBe(
      "graph TD\n  A[<script>alert('xss')</script>]",
    );
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
