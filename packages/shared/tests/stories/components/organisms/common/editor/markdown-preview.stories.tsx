import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { MarkdownPreview } from "@shared/components/organisms/common/editor";
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
  component: MarkdownPreview,
  args: {
    renderer: storybookRenderer,
  },
  decorators: [
    (Story) => (
      <div style={{ height: "600px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof MarkdownPreview>;

export default meta;

export const Empty: StoryObj<typeof MarkdownPreview> = {
  args: {
    title: "",
    content: "",
  },
};

export const TitleOnly: StoryObj<typeof MarkdownPreview> = {
  args: {
    title: "Next.js 15の新機能について",
    content: "",
  },
};

export const WithContent: StoryObj<typeof MarkdownPreview> = {
  args: {
    title: "Next.js 15の新機能について",
    content: `## 概要

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

### 命名規則

コンポーネントやファイルの命名には一貫性を持たせることが重要です。
以下の規則に従うことを推奨します。

- コンポーネントファイル: PascalCase (例: \`UserProfile.tsx\`)
- ユーティリティ関数: camelCase (例: \`formatDate.ts\`)
- 定数: UPPER_SNAKE_CASE (例: \`API_BASE_URL\`)
`,
  },
};

export const LongContent: StoryObj<typeof MarkdownPreview> = {
  args: {
    title: "React Hooksの完全ガイド",
    content: `## はじめに

React Hooksは、React 16.8で導入された機能で、関数コンポーネントで状態やライフサイクルを扱えるようになります。

## useState

最も基本的なHookで、コンポーネントに状態を追加できます。

\`\`\`typescript
const [count, setCount] = useState(0);
\`\`\`

## useEffect

副作用を扱うためのHookです。

\`\`\`typescript
useEffect(() => {
  document.title = \`Count: \${count}\`;
}, [count]);
\`\`\`

## useCallback

メモ化されたコールバック関数を返します。

\`\`\`typescript
const handleClick = useCallback(() => {
  console.log(count);
}, [count]);
\`\`\`

## useMemo

メモ化された値を返します。

\`\`\`typescript
const expensiveValue = useMemo(() => {
  return computeExpensiveValue(a, b);
}, [a, b]);
\`\`\`

## まとめ

React Hooksを使いこなすことで、より簡潔で再利用可能なコードを書くことができます。
`,
  },
};
