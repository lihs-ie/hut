import { visit } from "unist-util-visit";
import type { Root, Paragraph, Text, Link } from "mdast";
import type { Plugin } from "unified";

const URL_REGEX = /^https?:\/\/[^\s]+$/;

const isUrlOnlyParagraph = (node: Paragraph): string | null => {
  if (node.children.length !== 1) {
    return null;
  }

  const child = node.children[0];

  if (child.type === "text") {
    const text = (child as Text).value.trim();
    if (URL_REGEX.test(text)) {
      return text;
    }
  }

  if (child.type === "link") {
    const link = child as Link;
    if (link.children.length === 1 && link.children[0].type === "text") {
      const linkText = (link.children[0] as Text).value;
      if (linkText === link.url && URL_REGEX.test(link.url)) {
        return link.url;
      }
    }
  }

  return null;
};

export const remarkLinkCard: Plugin<[], Root> = () => {
  return (tree: Root) => {
    visit(tree, "paragraph", (node, index, parent) => {
      if (index === undefined || parent === undefined) {
        return;
      }

      const url = isUrlOnlyParagraph(node as Paragraph);
      if (!url) {
        return;
      }

      const linkCardNode = {
        type: "mdxJsxFlowElement",
        name: "LinkCard",
        attributes: [
          {
            type: "mdxJsxAttribute",
            name: "url",
            value: url,
          },
        ],
        children: [],
      };

      parent.children[index] = linkCardNode as unknown as Paragraph;
    });
  };
};

export default remarkLinkCard;
