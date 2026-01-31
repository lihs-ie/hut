import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SearchIndex } from "@shared/components/templates/search";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../../support/molds/domains/article";
import { MemoMold } from "../../../../support/molds/domains/memo";
import { SeriesMold } from "../../../../support/molds/domains/series";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";
import { Tag } from "@shared/domains/attributes/tag";
import { Article } from "@shared/domains/articles";
import { Memo } from "@shared/domains/memo";
import { Series } from "@shared/domains/series";

const meta = {
  component: SearchIndex,
  parameters: {
    nextjs: { appDirectory: true },
  },
} satisfies Meta<typeof SearchIndex>;

export default meta;

const tags = Forger(TagMold).forgeMultiWithSeed(10, 1);

const articles = Forger(ArticleMold).forgeMultiWithSeed(5, 1);
const memos = Forger(MemoMold).forgeMultiWithSeed(5, 2);
const seriesList = Forger(SeriesMold).forgeMultiWithSeed(3, 3);

const search = async (): Promise<(Article | Memo | Series)[]> => [
  ...articles,
  ...memos,
  ...seriesList,
];

const getAllTags = async (): Promise<Tag[]> => tags;

const findAllTags = async (identifiers: string[]): Promise<Tag[]> =>
  tags.filter((tag) => identifiers.includes(tag.identifier));

const ofNamesTags = async (names: string[]): Promise<Tag[]> =>
  tags.filter((tag) => names.includes(tag.name));

export const Default: StoryObj<typeof SearchIndex> = {
  args: {
    unvalidatedCriteria: {
      freeWord: null,
      tags: null,
      type: null,
      sortBy: null,
      order: null,
      limit: null,
    },
    search,
    getAllTags,
    findAllTags,
    ofNamesTags,
  },
};

export const WithFreeWord: StoryObj<typeof SearchIndex> = {
  args: {
    unvalidatedCriteria: {
      freeWord: "React",
      tags: null,
      type: null,
      sortBy: null,
      order: null,
      limit: null,
    },
    search,
    getAllTags,
    findAllTags,
    ofNamesTags,
  },
};

const searchEmpty = async (): Promise<(Article | Memo | Series)[]> => [];

export const NoResults: StoryObj<typeof SearchIndex> = {
  args: {
    unvalidatedCriteria: {
      freeWord: "存在しないキーワード",
      tags: null,
      type: null,
      sortBy: null,
      order: null,
      limit: null,
    },
    search: searchEmpty,
    getAllTags,
    findAllTags,
    ofNamesTags,
  },
};

const searchArticlesOnly = async (): Promise<(Article | Memo | Series)[]> =>
  articles;

export const ArticlesOnly: StoryObj<typeof SearchIndex> = {
  args: {
    unvalidatedCriteria: {
      freeWord: null,
      tags: null,
      type: "article",
      sortBy: null,
      order: null,
      limit: null,
    },
    search: searchArticlesOnly,
    getAllTags,
    findAllTags,
    ofNamesTags,
  },
};
