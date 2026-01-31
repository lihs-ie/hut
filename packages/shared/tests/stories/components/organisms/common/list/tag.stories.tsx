import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TagBadgeListPresenter } from "@shared/components/organisms/common/list/tag.presenter";
import { Forger } from "@lihs-ie/forger-ts";
import { TagMold } from "../../../../../support/molds/domains/attributes";

const meta = {
  component: TagBadgeListPresenter,
} satisfies Meta<typeof TagBadgeListPresenter>;

export default meta;

const createTags = (count: number) => Forger(TagMold).forgeMulti(count);

export const Default: StoryObj<typeof TagBadgeListPresenter> = {
  args: {
    tags: createTags(3),
  },
};

export const SingleTag: StoryObj<typeof TagBadgeListPresenter> = {
  args: {
    tags: createTags(1),
  },
};

export const ManyTags: StoryObj<typeof TagBadgeListPresenter> = {
  args: {
    tags: createTags(20),
  },
};

export const Empty: StoryObj<typeof TagBadgeListPresenter> = {
  args: {
    tags: [],
  },
};
