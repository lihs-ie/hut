import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesListIndex } from "@shared/components/templates/series/list";
import { Forger } from "@lihs-ie/forger-ts";
import { SeriesMold } from "../../../../support/molds/domains/series";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";
import { Tag } from "@shared/domains/attributes/tag";

const meta = {
  title: "templates/series/list-search",
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

const searchEmpty = async () => [];

export const Empty: StoryObj<typeof SeriesListIndex> = {
  args: {
    search: searchEmpty,
    findAllTags,
  },
};

const searchFew = async () => Forger(SeriesMold).forgeMultiWithSeed(2, 1);

export const FewItems: StoryObj<typeof SeriesListIndex> = {
  args: {
    search: searchFew,
    findAllTags,
  },
};
