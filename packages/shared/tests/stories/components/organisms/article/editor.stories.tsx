import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ArticleEditor } from "@/components/organisms/article/editor";

const meta = {
  component: ArticleEditor,
} satisfies Meta<typeof ArticleEditor>;
export default meta;

export const Default: StoryObj<typeof ArticleEditor> = {};
