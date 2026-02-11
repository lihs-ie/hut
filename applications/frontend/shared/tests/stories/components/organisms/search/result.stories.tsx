import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SearchResultPresenter } from "@shared/components/organisms/search/result.presenter";
import { ContentType } from "@shared/domains/search-token";
import { tagNameSchema, TagName } from "@shared/domains/attributes/tag";
import { Forger } from "@lihs-ie/forger-ts";
import { ArticleMold } from "../../../../support/molds/domains/article";
import { MemoMold } from "../../../../support/molds/domains/memo";
import { SeriesMold } from "../../../../support/molds/domains/series";
import { Article } from "@shared/domains/articles";
import { Memo } from "@shared/domains/memo";
import { Series } from "@shared/domains/series";

const meta = {
  component: SearchResultPresenter,
} satisfies Meta<typeof SearchResultPresenter>;

export default meta;

const sampleTagNames: TagName[] = [
  tagNameSchema.parse("TypeScript"),
  tagNameSchema.parse("React"),
  tagNameSchema.parse("Next.js"),
];

const articlesWithTagNames = (Forger(ArticleMold).forgeMulti(3) as Article[]).map(
  (article) => ({
    ...article,
    tagNames: sampleTagNames,
  })
);

const memosWithTagNames = (Forger(MemoMold).forgeMulti(2) as Memo[]).map(
  (memo) => ({
    ...memo,
    tagNames: sampleTagNames,
  })
);

const seriesWithTagNames = (Forger(SeriesMold).forgeMulti(2) as Series[]).map(
  (series) => ({
    ...series,
    tagNames: sampleTagNames,
  })
);

export const WithResults: StoryObj<typeof SearchResultPresenter> = {
  args: {
    contents: [...articlesWithTagNames, ...memosWithTagNames],
    criteria: {
      freeWord: "TypeScript",
      tags: null,
      type: null,
      sortBy: null,
      order: null,
      limit: null,
    },
  },
};

export const EmptyWithSearch: StoryObj<typeof SearchResultPresenter> = {
  args: {
    contents: [],
    criteria: {
      freeWord: "存在しないキーワード",
      tags: null,
      type: null,
      sortBy: null,
      order: null,
      limit: null,
    },
  },
};

export const InitialState: StoryObj<typeof SearchResultPresenter> = {
  args: {
    contents: [],
    criteria: {
      freeWord: null,
      tags: null,
      type: null,
      sortBy: null,
      order: null,
      limit: null,
    },
  },
};

export const MixedContent: StoryObj<typeof SearchResultPresenter> = {
  args: {
    contents: [
      ...articlesWithTagNames,
      ...memosWithTagNames,
      ...seriesWithTagNames,
    ],
    criteria: {
      freeWord: null,
      tags: ["tag-1"],
      type: null,
      sortBy: null,
      order: null,
      limit: null,
    },
  },
};

export const FilteredByType: StoryObj<typeof SearchResultPresenter> = {
  args: {
    contents: articlesWithTagNames,
    criteria: {
      freeWord: null,
      tags: null,
      type: ContentType.ARTICLE,
      sortBy: null,
      order: null,
      limit: null,
    },
  },
};

export const ManyResults: StoryObj<typeof SearchResultPresenter> = {
  args: {
    contents: (Forger(ArticleMold).forgeMulti(20) as Article[]).map(
      (article) => ({
        ...article,
        tagNames: sampleTagNames,
      })
    ),
    criteria: {
      freeWord: "search",
      tags: null,
      type: null,
      sortBy: null,
      order: null,
      limit: null,
    },
  },
};
