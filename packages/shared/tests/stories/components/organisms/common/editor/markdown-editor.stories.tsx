import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { MarkdownEditor } from "@shared/components/organisms/common/editor";

const meta = {
  component: MarkdownEditor,
  args: {
    onChange: () => {},
    onTagsChange: () => {},
  },
  decorators: [
    (Story) => (
      <div style={{ height: "600px", display: "flex" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof MarkdownEditor>;

export default meta;

export const Empty: StoryObj<typeof MarkdownEditor> = {
  args: {
    value: "",
    tags: [],
  },
};

export const WithContent: StoryObj<typeof MarkdownEditor> = {
  args: {
    value: `## 概要

Next.js 15とReact 19を使用したWebアプリケーション開発における、
基本的なルールとディレクトリ構成について解説します。

### ディレクトリ構成の基本方針

App Routerを前提とした場合、以下のようなディレクトリ構成を推奨します。

\`\`\`
app/
├── (marketing)/
│   ├── page.tsx
│   └── about/
│       └── page.tsx
├── (dashboard)/
│   ├── layout.tsx
│   └── dashboard/
│       └── page.tsx
└── layout.tsx
\`\`\`

### 命名規則

コンポーネントやファイルの命名には一貫性を持たせることが重要です。
`,
    tags: [],
  },
};

export const WithTags: StoryObj<typeof MarkdownEditor> = {
  args: {
    value: `## React Hooksの基礎

Hooksを使用することで、関数コンポーネントで状態やライフサイクルを扱えます。
`,
    tags: ["React", "Hooks", "TypeScript"],
  },
};

export const NoTagsSection: StoryObj<typeof MarkdownEditor> = {
  args: {
    value: "タグ入力欄がないバージョン",
    tags: undefined,
    onTagsChange: undefined,
  },
};
