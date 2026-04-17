import { visit } from "unist-util-visit";
import type { Root, Code } from "mdast";
import type { MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import type { Plugin } from "unified";
import { createMermaidRenderer, type MermaidRenderer } from "mermaid-isomorphic";
import { sanitizeMermaidSvg } from "../components/molecules/mermaid/sanitize";

let renderer: MermaidRenderer | null = null;

const getRenderer = (): MermaidRenderer => {
  if (!renderer) {
    renderer = createMermaidRenderer();
  }
  return renderer;
};

const createMermaidJsxNode = (
  html: string,
  fallback: boolean,
): MdxJsxFlowElement => ({
  type: "mdxJsxFlowElement",
  name: "MermaidSvg",
  attributes: [
    {
      type: "mdxJsxAttribute",
      name: "html",
      value: html,
    },
    ...(fallback
      ? [
          {
            type: "mdxJsxAttribute" as const,
            name: "fallback",
            value: null,
          },
        ]
      : []),
  ],
  children: [],
});

export const remarkMermaid: Plugin<[], Root> = () => {
  return async (tree: Root) => {
    const mermaidNodes: { node: Code; index: number }[] = [];

    visit(tree, "code", (node, index) => {
      if (node.lang !== "mermaid" || index === undefined) {
        return;
      }
      mermaidNodes.push({ node, index });
    });

    if (mermaidNodes.length === 0) {
      return;
    }

    const diagrams = mermaidNodes.map(({ node }) => node.value);
    const render = getRenderer();
    const results = await render(diagrams, {
      mermaidConfig: { theme: "default" },
    });

    for (let i = mermaidNodes.length - 1; i >= 0; i--) {
      const { index, node } = mermaidNodes[i];
      const result = results[i];

      if (result.status === "fulfilled" && result.value.svg) {
        const sanitized = sanitizeMermaidSvg(result.value.svg);
        tree.children[index] = createMermaidJsxNode(sanitized, false);
      } else {
        tree.children[index] = createMermaidJsxNode(node.value, true);
      }
    }
  };
};

export default remarkMermaid;
