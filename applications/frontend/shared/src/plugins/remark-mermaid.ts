import { visit } from "unist-util-visit";
import type { Root, Code } from "mdast";
import type { MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import type { Plugin } from "unified";

const createMermaidJsxNode = (code: string): MdxJsxFlowElement => ({
  type: "mdxJsxFlowElement",
  name: "MermaidClient",
  attributes: [
    {
      type: "mdxJsxAttribute",
      name: "code",
      value: code,
    },
  ],
  children: [],
});

export const remarkMermaid: Plugin<[], Root> = () => {
  return (tree: Root) => {
    const mermaidNodes: { node: Code; index: number }[] = [];

    visit(tree, "code", (node, index) => {
      if (node.lang !== "mermaid" || index === undefined) {
        return;
      }
      mermaidNodes.push({ node, index });
    });

    for (let i = mermaidNodes.length - 1; i >= 0; i--) {
      const { index, node } = mermaidNodes[i];
      tree.children[index] = createMermaidJsxNode(node.value);
    }
  };
};

export default remarkMermaid;
