import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TechStackList } from "@shared/components/molecules/list/tech-stack";
import { Forger } from "@lihs-ie/forger-ts";
import { TechnologyStackMold } from "../../../../support/molds/domains/common/tech";

const meta = {
  component: TechStackList,
} satisfies Meta<typeof TechStackList>;

export default meta;

export const Default: StoryObj<typeof TechStackList> = {
  args: {
    techStacks: new Map([
      ["frontend", Forger(TechnologyStackMold).forgeMulti(3)],
      ["backend", Forger(TechnologyStackMold).forgeMulti(3)],
    ]),
  },
};
