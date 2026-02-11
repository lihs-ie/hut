import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesChapters } from "@shared/components/organisms/series/detail/chapters";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ChapterMold,
  SeriesIdentifierMold,
} from "../../../../../support/molds/domains/series";
import { slugSchema } from "@shared/domains/common";
import { Chapter, SeriesIdentifier } from "@shared/domains/series";

const meta = {
  component: SeriesChapters,
} satisfies Meta<typeof SeriesChapters>;

export default meta;

const forgeChapters = (count: number): Chapter[] =>
  Forger(ChapterMold).forgeMulti(count) as Chapter[];

const forgeSeriesIdentifier = (): SeriesIdentifier =>
  Forger(SeriesIdentifierMold).forge() as SeriesIdentifier;

export const Default: StoryObj<typeof SeriesChapters> = {
  args: {
    chapters: forgeChapters(5),
    slug: "sample-series",
    series: forgeSeriesIdentifier(),
  },
};

export const SingleChapter: StoryObj<typeof SeriesChapters> = {
  args: {
    chapters: forgeChapters(1),
    slug: "single-chapter-series",
    series: forgeSeriesIdentifier(),
  },
};

export const ManyChapters: StoryObj<typeof SeriesChapters> = {
  args: {
    chapters: forgeChapters(15),
    slug: "long-series",
    series: forgeSeriesIdentifier(),
  },
};

export const WithCustomTitles: StoryObj<typeof SeriesChapters> = {
  args: {
    chapters: [
      Forger(ChapterMold).forge({
        title: "はじめに",
        slug: slugSchema.parse("introduction"),
      }) as Chapter,
      Forger(ChapterMold).forge({
        title: "基本概念",
        slug: slugSchema.parse("basics"),
      }) as Chapter,
      Forger(ChapterMold).forge({
        title: "応用編",
        slug: slugSchema.parse("advanced"),
      }) as Chapter,
      Forger(ChapterMold).forge({
        title: "まとめ",
        slug: slugSchema.parse("conclusion"),
      }) as Chapter,
    ],
    slug: "tutorial-series",
    series: forgeSeriesIdentifier(),
  },
};

export const Empty: StoryObj<typeof SeriesChapters> = {
  args: {
    chapters: [],
    slug: "empty-series",
    series: forgeSeriesIdentifier(),
  },
};
