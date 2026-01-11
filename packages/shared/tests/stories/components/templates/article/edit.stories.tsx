import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { ArticleEditTemplate } from "@shared/components/templates/article/edit";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import rehypeHighlight from "rehype-highlight";

// Storybook用の簡易markdownレンダラー（MDXRendererはRSC用なのでクライアントではreact-markdownを使用）
const storybookRenderer = (content: string) => (
  <ReactMarkdown remarkPlugins={[remarkGfm]} rehypePlugins={[rehypeHighlight]}>
    {content}
  </ReactMarkdown>
);

const meta = {
  component: ArticleEditTemplate,
  args: {
    onSave: async () => {},
    renderer: storybookRenderer,
  },
  parameters: {
    layout: "fullscreen",
  },
} satisfies Meta<typeof ArticleEditTemplate>;

export default meta;

export const NewArticle: StoryObj<typeof ArticleEditTemplate> = {
  args: {
    type: "article",
    backHref: "/admin/articles",
  },
};

export const EditArticle: StoryObj<typeof ArticleEditTemplate> = {
  args: {
    initialTitle: "Next.js 15の新機能について",
    initialContent: `## 概要

Next.js 15とReact 19を使用したWebアプリケーション開発における、
基本的なルールとディレクトリ構成について解説します。

### ディレクトリ構成の基本方針

App Routerを前提とした場合、以下のようなディレクトリ構成を推奨します。

\`\`\`typescript
// app/layout.tsx
export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="ja">
      <body>{children}</body>
    </html>
  )
}
\`\`\`
`,
    initialTags: ["Next.js", "React", "TypeScript"],
    initialPublished: false,
    type: "article",
    backHref: "/admin/articles",
  },
};

export const PublishedArticle: StoryObj<typeof ArticleEditTemplate> = {
  args: {
    initialTitle: "React Hooksの完全ガイド",
    initialContent: `## はじめに

React Hooksは、React 16.8で導入された機能です。

## useState

最も基本的なHookです。

\`\`\`typescript
const [count, setCount] = useState(0);
\`\`\`
`,
    initialTags: ["React", "Hooks"],
    initialPublished: true,
    type: "article",
    backHref: "/admin/articles",
  },
};

export const NewChapter: StoryObj<typeof ArticleEditTemplate> = {
  args: {
    type: "chapter",
    backHref: "/admin/books/my-book",
  },
};

export const EditChapter: StoryObj<typeof ArticleEditTemplate> = {
  args: {
    initialTitle: "第1章: はじめに",
    initialContent: `## この章で学ぶこと

この章では、基本的な概念について学びます。
`,
    initialPublished: false,
    type: "chapter",
    backHref: "/admin/books/my-book",
  },
};
