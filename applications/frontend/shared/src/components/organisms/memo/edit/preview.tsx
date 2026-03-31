"use client";

import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import remarkBreaks from "remark-breaks";
import { Prism as SyntaxHighlighter } from "react-syntax-highlighter";
import { oneDark } from "react-syntax-highlighter/dist/esm/styles/prism";
import { LinkCardClient } from "@shared/components/molecules/card/link.client";
import { extractStandaloneUrl } from "@shared/components/global/markdown-url";

type Props = {
  value: string;
};

export const EntryPreview = (props: Props) => {
  return (
    <ReactMarkdown
      remarkPlugins={[remarkGfm, remarkBreaks]}
      components={{
        p({ children }) {
          const url = extractStandaloneUrl(children);
          if (url) {
            return <LinkCardClient url={url} />;
          }
          return <p>{children}</p>;
        },
        code({ className, children }) {
          const match = /language-(\w+)/.exec(className || "");
          const isInline = !match && !className;

          return isInline ? (
            <code className={className}>
              {children}
            </code>
          ) : (
            <SyntaxHighlighter
              style={oneDark}
              language={match ? match[1] : "text"}
              PreTag="div"
            >
              {String(children).replace(/\n$/, "")}
            </SyntaxHighlighter>
          );
        },
      }}
    >
      {props.value}
    </ReactMarkdown>
  );
};
