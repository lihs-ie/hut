import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ChapterList } from "@shared/components/molecules/list/chapter";
import { Forger } from "@lihs-ie/forger-ts";
import {
  ChapterMold,
  ChapterSlugMold,
  SeriesIdentifierMold,
} from "../../../../support/molds/domains/series";

const meta = {
  component: ChapterList,
} satisfies Meta<typeof ChapterList>;

export default meta;

export const Default: StoryObj<typeof ChapterList> = {
  args: {
    chapters: Forger(ChapterMold).forgeMulti(5),
    series: Forger(SeriesIdentifierMold).forge(),
    slug: Forger(ChapterSlugMold).forge(),
  },
};
