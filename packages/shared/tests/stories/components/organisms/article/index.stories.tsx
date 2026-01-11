import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ContentPresenter } from "@shared/components/organisms/common/content.presenter";
import { MDXRenderer } from "@shared/components/global/mdx";
import { contentSchema, Article } from "@shared/domains/articles";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../../support/molds/domains/article";

const meta = {
  component: ContentPresenter<Article>,
} satisfies Meta<typeof ContentPresenter<Article>>;
export default meta;

const sampleContent = `
## Sample Article
This is a sample article written in **Markdown**.

### Section 1

Here is some sample content for section 1.

- Item 1
- Item 2
- Item 3

### Section 2

Here is some sample content for section 2.
`;

export const Default: StoryObj<typeof ContentPresenter<Article>> = {
  args: {
    target: Forger(ArticleMold).forge({
      content: contentSchema.parse(sampleContent),
    }),
    tagOf: (article) => article.tags,
    contentOf: (article) => article.content,
    renderer: (content: string) => MDXRenderer(content),
  },
};
