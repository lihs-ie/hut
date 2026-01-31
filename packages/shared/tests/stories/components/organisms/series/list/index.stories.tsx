import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesList } from "@shared/components/organisms/series/list";
import { Forger } from "@lihs-ie/forger-ts";
import { SeriesMold } from "../../../../../support/molds/domains/series";

const meta = {
  component: SeriesList,
} satisfies Meta<typeof SeriesList>;

export default meta;

const seriesList = Forger(SeriesMold).forgeMultiWithSeed(4, 1);

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
    seriesList: [Forger(SeriesMold).forge()],
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
    seriesList: Forger(SeriesMold).forgeMultiWithSeed(3, 2),
  },
};
