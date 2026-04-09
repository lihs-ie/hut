import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesListIndex } from "@shared/components/templates/series/list/index";
import { Forger } from "@lihs-ie/forger-ts";
import { SeriesMold } from "../../../../../support/molds/domains/series";
import { TagMold } from "../../../../../support/molds/domains/attributes/tag";
import { Tag } from "@shared/domains/attributes/tag";

const meta = {
  title: "templates/series/list-page",
  component: SeriesListIndex,
} satisfies Meta<typeof SeriesListIndex>;

export default meta;

const tags = Forger(TagMold).forgeMultiWithSeed(5, 1);

const search = async () => Forger(SeriesMold).forgeMultiWithSeed(5, 1);

const findAllTags = async (identifiers: string[]): Promise<Tag[]> =>
  tags.filter((tag) => identifiers.includes(tag.identifier));

export const Default: StoryObj<typeof SeriesListIndex> = {
  args: {
    search,
    findAllTags,
  },
};

export const Empty: StoryObj<typeof SeriesListIndex> = {
  args: {
    search: async () => [],
    findAllTags,
  },
};

export const SingleSeries: StoryObj<typeof SeriesListIndex> = {
  args: {
    search: async () => Forger(SeriesMold).forgeMultiWithSeed(1, 1),
    findAllTags,
  },
};

export const ManySeries: StoryObj<typeof SeriesListIndex> = {
  args: {
    search: async () => Forger(SeriesMold).forgeMultiWithSeed(10, 1),
    findAllTags,
  },
};
