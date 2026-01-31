import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesList } from "@shared/components/organisms/series/list";
import { Builder } from "../../../../../support/molds";
import { SeriesFactory } from "../../../../../support/molds/domains/series";

const meta = {
  component: SeriesList,
} satisfies Meta<typeof SeriesList>;

export default meta;

const seriesList = Builder(SeriesFactory).buildListWith(4, 1).toArray();

export const Default: StoryObj<typeof SeriesList> = {
  args: {
    seriesList,
    author: {
      name: "Author Name",
      avatar: "https://picsum.photos/seed/author/100/100",
    },
  },
};

export const SingleItem: StoryObj<typeof SeriesList> = {
  args: {
    seriesList: [Builder(SeriesFactory).build()],
    author: {
      name: "Single Author",
    },
  },
};

export const Empty: StoryObj<typeof SeriesList> = {
  args: {
    seriesList: [],
  },
};

export const WithoutAuthor: StoryObj<typeof SeriesList> = {
  args: {
    seriesList: Builder(SeriesFactory).buildListWith(3, 2).toArray(),
  },
};
