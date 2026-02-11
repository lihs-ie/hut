import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SeriesExplanation } from "@shared/components/organisms/series/detail/explanation";
import { SeriesMold } from "../../../../../support/molds/domains/series";
import { Forger } from "@lihs-ie/forger-ts";
import { TagIdentifierMold } from "../../../../../support/molds/domains/attributes/tag";

const meta = {
  component: SeriesExplanation,
} satisfies Meta<typeof SeriesExplanation>;

export default meta;

const series = Forger(SeriesMold).forge();
const tags = Forger(TagIdentifierMold).forgeMulti(3);

export const Default: StoryObj<typeof SeriesExplanation> = {
  args: {
    title: series.title,
    description: series.description,
    tags: tags,
  },
};
