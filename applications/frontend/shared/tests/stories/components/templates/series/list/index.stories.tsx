import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesListIndex } from "@shared/components/templates/series/list/index";
import { Forger } from "@lihs-ie/forger-ts";
import { SeriesMold } from "../../../../../support/molds/domains/series";

const meta = {
  title: "templates/series/list-page",
  component: SeriesListIndex,
} satisfies Meta<typeof SeriesListIndex>;

export default meta;

export const Default: StoryObj<typeof SeriesListIndex> = {
  args: {
    seriesList: Forger(SeriesMold).forgeMultiWithSeed(5, 1),
  },
};

export const Empty: StoryObj<typeof SeriesListIndex> = {
  args: {
    seriesList: [],
  },
};

export const SingleSeries: StoryObj<typeof SeriesListIndex> = {
  args: {
    seriesList: Forger(SeriesMold).forgeMultiWithSeed(1, 1),
  },
};

export const ManySeries: StoryObj<typeof SeriesListIndex> = {
  args: {
    seriesList: Forger(SeriesMold).forgeMultiWithSeed(10, 1),
  },
};
