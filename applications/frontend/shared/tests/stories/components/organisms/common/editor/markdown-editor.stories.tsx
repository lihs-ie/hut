import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MarkdownEditor } from "@shared/components/organisms/common/editor/markdown-editor";

const meta = {
  component: MarkdownEditor,
  args: {
    onChange: (value: string) => console.log("onChange:", value),
  },
} satisfies Meta<typeof MarkdownEditor>;

export default meta;

export const Default: StoryObj<typeof MarkdownEditor> = {
  args: {
    value: "",
    placeholder: "マークダウンで記述",
  },
};

export const WithContent: StoryObj<typeof MarkdownEditor> = {
  args: {
    value: `## サンプル記事

これは**Markdown**で書かれたサンプルです。

### セクション1

- アイテム1
- アイテム2
- アイテム3
`,
    placeholder: "マークダウンで記述",
  },
};

export const CustomPlaceholder: StoryObj<typeof MarkdownEditor> = {
  args: {
    value: "",
    placeholder: "メモを入力してください...",
  },
};

export const LongContent: StoryObj<typeof MarkdownEditor> = {
  args: {
    value: Array(20)
      .fill(null)
      .map(
        (_, index) =>
          `## セクション${index + 1}\n\nこれはセクション${index + 1}の内容です。\n`
      )
      .join("\n"),
    placeholder: "マークダウンで記述",
  },
};
