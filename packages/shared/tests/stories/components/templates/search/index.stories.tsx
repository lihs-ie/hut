import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SearchIndex } from "@shared/components/templates/search";
import { createSearchByIndexWorkflow } from "@shared/workflows/search-index";
import {
  ContentType,
  SearchIndexTitle,
  validateCriteria,
} from "@shared/domains/search-index/common";
import { Logger } from "@shared/aspects/logger";
import { score } from "@shared/aspects/ngram";
import {
  ArticleMold,
  ArticleRepositoryMold,
} from "../../../../support/molds/domains/article";
import {
  SeriesMold,
  SeriesRepositoryMold,
} from "../../../../support/molds/domains/series";
import {
  MemoMold,
  MemoRepositoryMold,
} from "../../../../support/molds/domains/memo";
import {
  SearchIndexMold,
  SearchIndexRepositoryMold,
} from "../../../../support/molds/domains/search-index";
import { Forger } from "@lihs-ie/forger-ts";
import { Tag } from "@shared/domains/attributes/tag";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";

const meta = {
  component: SearchIndex,
  parameters: {
    layout: "fullscreen",
    nextjs: {
      appDirectory: true,
    },
  },
} satisfies Meta<typeof SearchIndex>;

export default meta;

const articles = Forger(ArticleMold).forgeMulti(20);
const series = Forger(SeriesMold).forgeMulti(20);
const memos = Forger(MemoMold).forgeMulti(20);
const tags = Forger(TagMold).forgeMulti(10);
const searchIndices = [...articles, ...series, ...memos].map((content) =>
  Forger(SearchIndexMold).forge({
    reference: content.identifier,
    title: content.title as SearchIndexTitle,
    timeline: content.timeline,
    tags: content.tags,
  })
);

const indexRepository = Forger(SearchIndexRepositoryMold).forge({
  instances: searchIndices,
});
const articleRepository = Forger(ArticleRepositoryMold).forge({
  instances: articles,
});
const seriesRepository = Forger(SeriesRepositoryMold).forge({
  instances: series,
});
const memoRepository = Forger(MemoRepositoryMold).forge({
  instances: memos,
});

const search = createSearchByIndexWorkflow(validateCriteria)(
  Logger("development")
)(score(() => 1))(indexRepository.search)(articleRepository.ofIdentifiers)(
  memoRepository.ofIdentifiers
)(seriesRepository.ofIdentifiers);

/**
 * Resolves tags for Storybook fixtures.
 */
const findAllTags = async (identifiers: string[]): Promise<Tag[]> =>
  tags.filter((tag) => identifiers.includes(tag.identifier));

/**
 * Provides tags for Storybook fixtures.
 */
const getAllTags = async (): Promise<Tag[]> => tags;

export const Default: StoryObj<typeof SearchIndex> = {
  args: {
    search,
    getAllTags,
    findAllTags,
    unvalidatedCriteria: {
      freeWord: null,
      tags: null,
      type: ContentType.ALL,
      sortBy: null,
      order: null,
    },
  },
};
