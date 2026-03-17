"use client";

import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import remarkBreaks from "remark-breaks";
import remarkMath from "remark-math";
import rehypeSlug from "rehype-slug";
import rehypeKatex from "rehype-katex";
import { Prism as SyntaxHighlighter } from "react-syntax-highlighter";
import {
  oneLight,
  oneDark,
} from "react-syntax-highlighter/dist/esm/styles/prism";
import styles from "./markdown-preview.module.css";
import codeBlockStyles from "@shared/components/global/mdx.module.css";
import { stripFrontmatter } from "@shared/components/global/matter";
import { LinkCardClient } from "@shared/components/molecules/card/link.client";
import { ContentImage } from "@shared/components/atoms/image/content";
import React, { useEffect, useRef } from "react";
import { parseMessageBox, parseAccordion, parseCodeBlockFilename, parseImageWidth } from "./markdown-extension";
import { sanitizeMermaidSvg } from "@shared/components/molecules/mermaid/sanitize";

const escapeHtml = (text: string): string => {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");
};

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

const preprocessZennMarkdown = (content: string): string => {
  let processed = parseMessageBox(content);
  processed = parseAccordion(processed);
  processed = parseCodeBlockFilename(processed);
  processed = parseImageWidth(processed);
  return processed;
};

export type Props = {
  content: string;
  title: string;
};

export const MarkdownPreview = (props: Props) => {
  const strippedContent = stripFrontmatter(props.content);
  const processedContent = preprocessZennMarkdown(strippedContent);
  const containerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (typeof window === "undefined") return;

    const mermaidBlocks = containerRef.current?.querySelectorAll(".language-mermaid");
    if (!mermaidBlocks || mermaidBlocks.length === 0) return;

    import("mermaid").then((module) => {
      const mermaid = module.default;
      mermaid.initialize({ startOnLoad: false, theme: "default" });

      const renderPromises = Array.from(mermaidBlocks).map(async (block) => {
        const code = block.textContent ?? "";
        if (code.length > 2000) return;

        const mermaidContainer = block.closest("[data-mermaid-container]");
        if (!mermaidContainer) return;

        const mermaidId = `mermaid-${Math.random().toString(36).slice(2)}`;

        try {
          const { svg } = await mermaid.render(mermaidId, code);
          mermaidContainer.innerHTML = sanitizeMermaidSvg(svg);
        } catch {
          document.getElementById(mermaidId)?.remove();
          document.getElementById(`d${mermaidId}`)?.remove();
          mermaidContainer.innerHTML = `<pre>${escapeHtml(code)}</pre>`;
        }
      });
      void Promise.all(renderPromises);
    });
  }, [processedContent]);

  return (
    <div className={styles.container} ref={containerRef}>
      <div className={styles.card}>
        {props.title && <h1 className={styles.title}>{props.title}</h1>}
        <div className={`prose ${styles.content}`}>
          {props.content ? (
            <ReactMarkdown
              remarkPlugins={[remarkGfm, remarkBreaks, remarkMath]}
              rehypePlugins={[rehypeSlug, rehypeKatex]}
              components={{
                img: (
                  imageProps: React.ImgHTMLAttributes<HTMLImageElement>
                ) => {
                  const src = typeof imageProps.src === "string" ? imageProps.src : "";
                  if (!src || src.startsWith("placeholder-")) {
                    return (
                      <span style={{ color: "var(--muted-foreground)", fontStyle: "italic" }}>
                        {imageProps.alt ?? "画像をアップロード中..."}
                      </span>
                    );
                  }
                  return (
                    <ContentImage
                      src={src}
                      alt={imageProps.alt ?? ""}
                    />
                  );
                },
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
                code({ className, children }) {
                  const match = /language-(\w+)/.exec(className || "");
                  const language = match ? match[1] : "";
                  const isInline = !match;

                  if (isInline) {
                    return (
                      <code className={className}>
                        {children}
                      </code>
                    );
                  }

                  if (language === "mermaid") {
                    return (
                      <div data-mermaid-container="true">
                        <code className={`language-mermaid ${className ?? ""}`}>
                          {children}
                        </code>
                      </div>
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
              {processedContent}
            </ReactMarkdown>
          ) : (
            <p className={styles.empty}>コンテンツがありません</p>
          )}
        </div>
      </div>
    </div>
  );
};
