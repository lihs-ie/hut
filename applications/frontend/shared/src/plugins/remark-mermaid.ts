import { visit } from "unist-util-visit";
import type { Root, Code, Html } from "mdast";
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

const escapeHtml = (text: string): string =>
  text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");

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

      let htmlValue: string;
      if (result.status === "fulfilled" && result.value.svg) {
        const sanitized = sanitizeMermaidSvg(result.value.svg);
        htmlValue = `<div class="mermaid-svg">${sanitized}</div>`;
      } else {
        const rawCode = escapeHtml(node.value);
        htmlValue = `<pre class="mermaid-svg fallback"><code>${rawCode}</code></pre>`;
      }

      const htmlNode: Html = {
        type: "html",
        value: htmlValue,
      };
      tree.children[index] = htmlNode;
    }
  };
};

export default remarkMermaid;
