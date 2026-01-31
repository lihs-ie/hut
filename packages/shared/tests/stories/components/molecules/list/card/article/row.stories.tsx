import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ArticleRowCard } from "@shared/components/molecules/list/card/article/row";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../../../../support/molds/domains/article";

const meta = {
  component: ArticleRowCard,
} satisfies Meta<typeof ArticleRowCard>;
export default meta;

export const Default: StoryObj<typeof ArticleRowCard> = {
  args: {
    article: Forger(ArticleMold).forge(),
  },
};
