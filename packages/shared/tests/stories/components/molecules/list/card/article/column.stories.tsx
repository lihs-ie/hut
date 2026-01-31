import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ArticleColumnCard } from "@shared/components/molecules/list/card/article/column";
import { Builder } from "../../../../../../support/molds";
import { ArticleFactory } from "../../../../../../support/molds/domains/article";

const meta = {
  component: ArticleColumnCard,
} satisfies Meta<typeof ArticleColumnCard>;
export default meta;

export const Default: StoryObj<typeof ArticleColumnCard> = {
  args: {
    article: Builder(ArticleFactory).build(),
  },
};
