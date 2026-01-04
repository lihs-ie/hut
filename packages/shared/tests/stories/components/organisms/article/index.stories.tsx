import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ArticlePresenter } from "@/components/organisms/article/index.presenter";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import { Builder } from "../../../../factories";
import { ArticleFactory } from "../../../../factories/domains/article";
import { contentSchema } from "@/domains/articles";

const meta = {
  component: ArticlePresenter,
} satisfies Meta<typeof ArticlePresenter>;
export default meta;

export const Default: StoryObj<typeof ArticlePresenter> = {
  args: {
    article: Builder(ArticleFactory).build({
      content: contentSchema.parse(`
# Sample Article
This is a sample article written in **Markdown**.

## Section 1

Here is some sample content for section 1.

- Item 1
- Item 2
- Item 3

## Section 2

Here is some sample content for section 2.
      `),
    }),
    renderer: (article) => (
      <ReactMarkdown remarkPlugins={[remarkGfm]}>
        {article.content}
      </ReactMarkdown>
    ),
  },
};
