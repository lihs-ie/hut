import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ArticleListIndex } from "@shared/components/templates/article/list";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../../support/molds/domains/article";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";
import { Tag } from "@shared/domains/attributes/tag";

const meta = {
  component: ArticleListIndex,
} satisfies Meta<typeof ArticleListIndex>;

export default meta;

const tags = Forger(TagMold).forgeMultiWithSeed(5, 1);

const search = async () => Forger(ArticleMold).forgeMultiWithSeed(10, 1);

const findAllTags = async (identifiers: string[]): Promise<Tag[]> =>
  tags.filter((tag) => identifiers.includes(tag.identifier));

export const Default: StoryObj<typeof ArticleListIndex> = {
  args: {
    search,
    findAllTags,
  },
};

const searchEmpty = async () => [];

export const Empty: StoryObj<typeof ArticleListIndex> = {
  args: {
    search: searchEmpty,
    findAllTags,
  },
};

const searchFew = async () => Forger(ArticleMold).forgeMultiWithSeed(3, 1);

export const FewItems: StoryObj<typeof ArticleListIndex> = {
  args: {
    search: searchFew,
    findAllTags,
  },
};

export const EditLinkMode: StoryObj<typeof ArticleListIndex> = {
  args: {
    search,
    findAllTags,
    linkMode: "edit",
  },
};
