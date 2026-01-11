import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ArticleIndex } from "@shared/components/templates/article";

import { generateToc, MDXRenderer } from "@shared/components/global/mdx";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ArticleMold,
  ContentMold,
  SlugMold,
} from "../../../../support/molds/domains/article";

const meta = {
  component: ArticleIndex,
} satisfies Meta<typeof ArticleIndex>;

export default meta;

const content = `# Sample Article

This is a sample article written in MDX.

- Point 1
- Point 2
- Point 3

## Conclusion

Thank you for reading this sample article.

### Subtitle

Here is some code:

## Heading 1

### Subheading 1.1

Some content under subheading 1.1.

`;

const find = async (slug: string) =>
  Forger(ArticleMold).forge({
    content: Forger(ContentMold).forge({ value: content }),
    slug: Forger(SlugMold).forge({ value: slug }),
  });

export const Default: StoryObj<typeof ArticleIndex> = {
  args: {
    slug: "sample-article",
    article: {
      find,
      renderer: (content: string) => MDXRenderer(content),
    },
    sidebar: {
      createTableTree: async (_: string) => generateToc(content),
    },
  },
};
