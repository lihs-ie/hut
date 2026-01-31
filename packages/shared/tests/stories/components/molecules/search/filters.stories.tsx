import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SearchFilters } from "@shared/components/organisms/search/filters";
import { Forger } from "@lihs-ie/forger-ts";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";

const meta = {
  component: SearchFilters,
  parameters: {
    layout: "padded",
    nextjs: {
      appDirectory: true,
    },
  },
} satisfies Meta<typeof SearchFilters>;

export default meta;

export const Default: StoryObj<typeof SearchFilters> = {
  args: {
    tags: Forger(TagMold).forgeMulti(10),
  },
};
