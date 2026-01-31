import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ChapterTitleCard } from "@shared/components/molecules/list/card/series/chapter-title";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ChapterMold,
  SeriesIdentifierMold,
} from "../../../../../../support/molds/domains/series";

const meta = {
  component: ChapterTitleCard,
} satisfies Meta<typeof ChapterTitleCard>;

export default meta;

const chapter = Forger(ChapterMold).forge();

export const Default: StoryObj<typeof ChapterTitleCard> = {
  args: {
    title: chapter.title,
    slug: chapter.slug,
    series: Forger(SeriesIdentifierMold).forge(),
    index: 1,
  },
};
