import { Article } from "@/domains/articles";
import { ArticlePersistWorkflow } from "@/workflows/article";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import rehypeHighlight from "rehype-highlight";
import { useState } from "react";
import styles from "./editor.module.css";

export type Props = {
  persist: ArticlePersistWorkflow;
  initial?: Article;
};

export const ArticleEditor = (props: Props) => {
  const [content, setContent] = useState<string>(props.initial?.content || "");

  return (
    <div className={styles.container}>
      <div className={styles.input}>
        <textarea
          className={styles.textarea}
          value={content}
          onChange={(e) => setContent(e.target.value)}
        ></textarea>
      </div>
      <div className={styles.markdown}>
        <ReactMarkdown
          remarkPlugins={[remarkGfm]}
          rehypePlugins={[rehypeHighlight]}
        >
          {content}
        </ReactMarkdown>
      </div>
    </div>
  );
};
