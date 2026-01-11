import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TagSelect } from "@shared/components/molecules/select/tag";
import { Forger } from "@lihs-ie/forger-ts";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";

const meta = {
  component: TagSelect,
} satisfies Meta<typeof TagSelect>;

export default meta;

export const Default: StoryObj<typeof TagSelect> = {
  args: {
    tags: Forger(TagMold).forgeMulti(10),
    selected: [],
    onSelect: (tag) => {
      console.log("Selected tag:", tag);
    },
  },
};
