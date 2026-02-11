import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SearchFilterPresenter } from "@shared/components/organisms/search/filters.presenter";
import { Forger } from "@lihs-ie/forger-ts";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";

const meta = {
  component: SearchFilterPresenter,
  parameters: {
    layout: "padded",
    nextjs: {
      appDirectory: true,
    },
  },
} satisfies Meta<typeof SearchFilterPresenter>;

export default meta;

export const Default: StoryObj<typeof SearchFilterPresenter> = {
  args: {
    tags: Forger(TagMold).forgeMulti(10),
  },
};
