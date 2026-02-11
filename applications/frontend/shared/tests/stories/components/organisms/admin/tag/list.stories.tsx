import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TagListPresenter } from "../../../../../../../admin/src/app/admin/_components/organisms/admin/tag/list.presenter";
import { TagMold } from "../../../../../support/molds/domains/attributes/tag";
import { Forger } from "@lihs-ie/forger-ts";

const meta = {
  component: TagListPresenter,
  parameters: {
    layout: "fullscreen",
  },
} satisfies Meta<typeof TagListPresenter>;
export default meta;

type Story = StoryObj<typeof TagListPresenter>;

const tagForger = Forger(TagMold);

export const Default: Story = {
  args: {
    tags: tagForger.forgeMulti(5),
  },
};

export const Empty: Story = {
  args: {
    tags: [],
  },
};

export const ManyTags: Story = {
  args: {
    tags: tagForger.forgeMulti(12),
  },
};

export const WithRealNames: Story = {
  args: {
    tags: [
      tagForger.forge(),
      tagForger.forge(),
      tagForger.forge(),
      tagForger.forge(),
      tagForger.forge(),
    ],
  },
};
