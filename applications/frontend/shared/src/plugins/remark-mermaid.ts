import { visit } from "unist-util-visit";
import type { Root, Code } from "mdast";
import type { Plugin } from "unified";
import { createMermaidRenderer } from "mermaid-isomorphic";
import { sanitizeMermaidSvg } from "@shared/components/molecules/mermaid/sanitize";

let renderer: ReturnType<typeof createMermaidRenderer> | null = null;

const getRenderer = () => {
  if (!renderer) {
    renderer = createMermaidRenderer();
  }
  return renderer;
};

export const remarkMermaid: Plugin<[], Root> = () => {
  return async (tree: Root) => {
    const mermaidNodes: { node: Code; index: number }[] = [];

    visit(tree, "code", (node, index) => {
      if (node.lang !== "mermaid" || index === undefined) {
        return;
      }
      mermaidNodes.push({ node: node as Code, index });
    });

    if (mermaidNodes.length === 0) {
      return;
    }

    const diagrams = mermaidNodes.map(({ node }) => node.value);
    const render = getRenderer();
    const results = await render(diagrams, {
      mermaidConfig: { theme: "default", startOnLoad: false },
    });

    for (let i = mermaidNodes.length - 1; i >= 0; i--) {
      const { index } = mermaidNodes[i];
      const result = results[i];

      let htmlValue: string;
      if (result.status === "fulfilled" && result.value.svg) {
        const sanitized = sanitizeMermaidSvg(result.value.svg);
        htmlValue = `<div class="mermaid-svg">${sanitized}</div>`;
      } else {
        const rawCode = mermaidNodes[i].node.value;
        htmlValue = `<pre class="mermaid-svg fallback"><code>${rawCode}</code></pre>`;
      }

      tree.children[index] = {
        type: "html",
        value: htmlValue,
      } as unknown as Code;
    }
  };
};

export default remarkMermaid;
