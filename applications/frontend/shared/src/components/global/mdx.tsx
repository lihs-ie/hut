import { MDXRemote, MDXRemoteProps } from "next-mdx-remote/rsc";
import Image from "next/image";
import rehypeShiki from "@shikijs/rehype";
import remarkGfm from "remark-gfm";
import rehypeSlug from "rehype-slug";
import React from "react";
import styles from "./mdx.module.css";
import { remarkLinkCard } from "@shared/plugins/remark-link-card";
import { LinkCard } from "@shared/components/molecules/card/link";

export type MarkdownRenderer = (content: string) => React.ReactNode;

const mdxOptions: MDXRemoteProps["options"] = {
  mdxOptions: {
    remarkPlugins: [remarkGfm, remarkLinkCard],
    rehypePlugins: [
      [
        rehypeShiki,
        {
          theme: "github-dark",
        },
      ],
      rehypeSlug,
    ],
  },
  parseFrontmatter: true,
};

const languageLabels: Record<string, string> = {
  typescript: "TypeScript",
  ts: "TypeScript",
  go: "Go",
  rust: "Rust",
  haskell: "Haskell",
  hs: "Haskell",
  shell: "Shell",
  bash: "Bash",
  sh: "Shell",
  sql: "SQL",
};

type PreProps = React.HTMLAttributes<HTMLPreElement> & {
  children?: React.ReactNode;
};

const extractLanguage = (children: React.ReactNode): string | null => {
  if (!React.isValidElement(children)) return null;

  const childProps = children.props as { className?: string };
  const className = childProps.className || "";
  const match = className.match(/language-(\w+)/);

  return match ? match[1] : null;
};

const CodeBlock = (props: PreProps) => {
  const language = extractLanguage(props.children);
  const displayLanguage = language
    ? languageLabels[language] || language
    : null;

  return (
    <div className={styles["code-block"]}>
      {displayLanguage && (
        <div className={styles["code-block-header"]}>
          <span className={styles["code-block-language"]}>
            {displayLanguage}
          </span>
        </div>
      )}
      <pre className={styles["code-block-pre"]} {...props}>
        {props.children}
      </pre>
    </div>
  );
};

const mdxComponents = {
  h1: (props: React.HTMLAttributes<HTMLHeadingElement>) => <h1 {...props} />,
  h2: (props: React.HTMLAttributes<HTMLHeadingElement>) => <h2 {...props} />,
  img: (props: React.ImgHTMLAttributes<HTMLImageElement>) => (
    <Image
      src={props.src as string}
      alt={props.alt ?? ""}
      width={800}
      height={450}
    />
  ),
  pre: CodeBlock,
  LinkCard: (props: { url: string }) => <LinkCard url={props.url} />,
};

export const MDXRenderer: MarkdownRenderer = (
  content: string,
  options: MDXRemoteProps["options"] = mdxOptions,
) => (
  <MDXRemote source={content} options={options} components={mdxComponents} />
);

// 目次
export type Heading = {
  level: number;
  text: string;
  identifier: string;
};

const slugify = (text: string) =>
  text
    .toLowerCase()
    .trim()
    .replace(/[^\p{L}\p{N}\s-]/gu, "")
    .replace(/\s+/g, "-");

const extractHeadings = (mdx: string): Heading[] => {
  const lines = mdx.split("\n");
  const headings: Heading[] = [];

  let inCodeBlock = false;

  for (const line of lines) {
    if (line.startsWith("```")) {
      inCodeBlock = !inCodeBlock;
      continue;
    }
    if (inCodeBlock) continue;

    const match = line.match(/^(#{2,6})\s+(.*)$/);
    if (!match) continue;

    const level = match[1].length;
    const text = match[2].trim();

    headings.push({
      level,
      text,
      identifier: slugify(text),
    });
  }

  return headings;
};

export type Node = {
  headline: string;
  identifier: string;
  children: Node[];
};

const buildTocTree = (headings: Heading[]): Node[] => {
  const root: Node[] = [];
  const stack: { level: number; node: Node }[] = [];

  for (const h of headings) {
    const node: Node = {
      headline: h.text,
      identifier: h.identifier,
      children: [],
    };

    while (stack.length > 0 && stack[stack.length - 1].level >= h.level) {
      stack.pop();
    }

    if (stack.length === 0) {
      root.push(node);
    } else {
      stack[stack.length - 1].node.children.push(node);
    }

    stack.push({ level: h.level, node });
  }

  return root;
};

export const generateToc = (mdx: string): Node[] => {
  const headings = extractHeadings(mdx);

  return buildTocTree(headings);
};
