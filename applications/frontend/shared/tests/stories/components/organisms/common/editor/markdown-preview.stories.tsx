import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MarkdownPreview } from "@shared/components/organisms/common/editor/markdown-preview";

const meta = {
  component: MarkdownPreview,
} satisfies Meta<typeof MarkdownPreview>;

export default meta;

const sampleMarkdown = `## サンプル記事

これは**Markdown**で書かれたサンプル記事です。

### セクション1

サンプルコンテンツです。

- アイテム1
- アイテム2
- アイテム3

### コードブロック

\`\`\`typescript
const greeting = (name: string): string => {
  return \`Hello, \${name}!\`;
};
\`\`\`

### リンク

[サンプルリンク](https://example.com)
`;

export const Default: StoryObj<typeof MarkdownPreview> = {
  args: {
    title: "サンプルタイトル",
    content: sampleMarkdown,
  },
};

export const WithoutTitle: StoryObj<typeof MarkdownPreview> = {
  args: {
    title: "",
    content: sampleMarkdown,
  },
};

export const EmptyContent: StoryObj<typeof MarkdownPreview> = {
  args: {
    title: "空のコンテンツ",
    content: "",
  },
};

export const LongContent: StoryObj<typeof MarkdownPreview> = {
  args: {
    title: "長いコンテンツ",
    content: `## 長い記事

${Array(10)
  .fill(null)
  .map(
    (_, index) => `### セクション${index + 1}

これはセクション${index + 1}のコンテンツです。サンプルテキストを繰り返し表示しています。

- ポイント1
- ポイント2
- ポイント3

`
  )
  .join("")}`,
  },
};

export const WithCodeBlocks: StoryObj<typeof MarkdownPreview> = {
  args: {
    title: "コードブロック付き記事",
    content: `## プログラミングガイド

### TypeScript

\`\`\`typescript
interface User {
  id: string;
  name: string;
  email: string;
}

const createUser = (data: Partial<User>): User => {
  return {
    id: data.id ?? crypto.randomUUID(),
    name: data.name ?? "Unknown",
    email: data.email ?? "",
  };
};
\`\`\`

### Go

\`\`\`go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
\`\`\`

### SQL

\`\`\`sql
SELECT * FROM users WHERE status = 'active' ORDER BY created_at DESC;
\`\`\`
`,
  },
};
