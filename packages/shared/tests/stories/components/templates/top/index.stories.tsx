import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TopIndex } from "@shared/components/templates/top";
import { Builder } from "../../../../support/molds";
import { MemoFactory } from "../../../../support/molds/domains/memo";
import { ProfileFactory } from "../../../../support/molds/domains/user";

const meta = {
  component: TopIndex,
} satisfies Meta<typeof TopIndex>;

export default meta;

const searchMemos = async () => Builder(MemoFactory).buildList(10).toArray();
const getProfile = async () => Builder(ProfileFactory).build();

export const Default: StoryObj<typeof TopIndex> = {
  args: {
    searchMemos,
    getProfile,
  },
};
