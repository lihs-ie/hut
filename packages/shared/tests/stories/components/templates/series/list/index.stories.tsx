import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesListIndex } from "@shared/components/templates/series/list";
import { Builder } from "../../../../../support/molds";
import { SeriesFactory } from "../../../../../support/molds/domains/series";

const meta = {
  component: SeriesListIndex,
} satisfies Meta<typeof SeriesListIndex>;

export default meta;

const seriesList = Builder(SeriesFactory).buildListWith(6, 1).toArray();

export const Default: StoryObj<typeof SeriesListIndex> = {
  args: {
    seriesList,
    author: {
      name: "Author Name",
      avatar: "https://picsum.photos/seed/author/100/100",
    },
  },
};

export const Empty: StoryObj<typeof SeriesListIndex> = {
  args: {
    seriesList: [],
  },
};

export const SingleSeries: StoryObj<typeof SeriesListIndex> = {
  args: {
    seriesList: [Builder(SeriesFactory).build()],
    author: {
      name: "Single Author",
    },
  },
};
