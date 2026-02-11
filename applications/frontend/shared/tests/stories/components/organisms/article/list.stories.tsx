import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ArticleListPresenter } from "@shared/components/organisms/article/list.presenter";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../../support/molds/domains/article";

const meta = {
  component: ArticleListPresenter,
} satisfies Meta<typeof ArticleListPresenter>;

export default meta;

export const Default: StoryObj<typeof ArticleListPresenter> = {
  args: {
    articles: Forger(ArticleMold).forgeMulti(5),
  },
};

export const SingleArticle: StoryObj<typeof ArticleListPresenter> = {
  args: {
    articles: Forger(ArticleMold).forgeMulti(1),
  },
};

export const Empty: StoryObj<typeof ArticleListPresenter> = {
  args: {
    articles: [],
  },
};

export const ManyArticles: StoryObj<typeof ArticleListPresenter> = {
  args: {
    articles: Forger(ArticleMold).forgeMulti(12),
  },
};
