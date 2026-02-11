import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import remarkBreaks from "remark-breaks";
import rehypeSlug from "rehype-slug";
import { Prism as SyntaxHighlighter } from "react-syntax-highlighter";
import {
  oneLight,
  oneDark,
} from "react-syntax-highlighter/dist/esm/styles/prism";
import styles from "./markdown-preview.module.css";
import codeBlockStyles from "@shared/components/global/mdx.module.css";
import { stripFrontmatter } from "@shared/components/global/matter";
import { LinkCardClient } from "@shared/components/molecules/card/link.client";
import React from "react";

const languageLabels: Record<string, string> = {
  typescript: "TypeScript",
  ts: "TypeScript",
  tsx: "TSX",
  javascript: "JavaScript",
  js: "JavaScript",
  jsx: "JSX",
  go: "Go",
  rust: "Rust",
  haskell: "Haskell",
  hs: "Haskell",
  shell: "Shell",
  bash: "Bash",
  sh: "Shell",
  sql: "SQL",
};

export type Props = {
  content: string;
  title: string;
};

export const MarkdownPreview = (props: Props) => {
  const strippedContent = stripFrontmatter(props.content);

  return (
    <div className={styles.container}>
      <div className={styles.card}>
        {props.title && <h1 className={styles.title}>{props.title}</h1>}
        <div className={`prose ${styles.content}`}>
          {props.content ? (
            <ReactMarkdown
              remarkPlugins={[remarkGfm, remarkBreaks]}
              rehypePlugins={[rehypeSlug]}
              components={{
                p({ children }) {
                  if (React.Children.count(children) === 1) {
                    const child = React.Children.toArray(children)[0];
                    if (typeof child === "string") {
                      const trimmed = child.trim();
                      if (/^https?:\/\/[^\s]+$/.test(trimmed)) {
                        return <LinkCardClient url={trimmed} />;
                      }
                    }
                  }
                  return <p>{children}</p>;
                },
                code({ className, children, ...rest }) {
                  const match = /language-(\w+)/.exec(className || "");
                  const language = match ? match[1] : "";
                  const isInline = !match;

                  if (isInline) {
                    return (
                      <code className={className} {...rest}>
                        {children}
                      </code>
                    );
                  }

                  const displayLanguage =
                    languageLabels[language] || language || null;
                  const isDark =
                    typeof document !== "undefined" &&
                    document.documentElement.classList.contains("dark");

                  return (
                    <div className={codeBlockStyles["code-block"]}>
                      {displayLanguage && (
                        <div className={codeBlockStyles["code-block-header"]}>
                          <span
                            className={codeBlockStyles["code-block-language"]}
                          >
                            {displayLanguage}
                          </span>
                        </div>
                      )}
                      <SyntaxHighlighter
                        style={isDark ? oneDark : oneLight}
                        language={language}
                        PreTag="div"
                        className={codeBlockStyles["code-block-pre"]}
                        customStyle={{
                          margin: 0,
                        }}
                      >
                        {String(children).replace(/\n$/, "")}
                      </SyntaxHighlighter>
                    </div>
                  );
                },
              }}
            >
              {strippedContent}
            </ReactMarkdown>
          ) : (
            <p className={styles.empty}>コンテンツがありません</p>
          )}
        </div>
      </div>
    </div>
  );
};
