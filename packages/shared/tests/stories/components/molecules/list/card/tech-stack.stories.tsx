import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TechStackCard } from "@shared/components/molecules/list/card/tech-stack";
import { Forger } from "@lihs-ie/forger-ts";
import { TechnologyStackMold } from "../../../../../support/molds/domains/common/tech";

const meta = {
  component: TechStackCard,
} satisfies Meta<typeof TechStackCard>;

export default meta;

export const Default: StoryObj<typeof TechStackCard> = {
  args: {
    techStack: Forger(TechnologyStackMold).forge(),
  },
};
