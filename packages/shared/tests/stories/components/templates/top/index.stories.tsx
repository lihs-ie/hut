import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TopIndex } from "@shared/components/templates/top";
import { Forger } from "@lihs-ie/forger-ts";
import { MemoMold } from "../../../../support/molds/domains/memo";
import { ProfileMold } from "../../../../support/molds/domains/user";

const meta = {
  component: TopIndex,
} satisfies Meta<typeof TopIndex>;

export default meta;

const searchMemos = async () => Forger(MemoMold).forgeMultiWithSeed(10, 1);
const getProfile = async () => Forger(ProfileMold).forge();

export const Default: StoryObj<typeof TopIndex> = {
  args: {
    searchMemos,
    getProfile,
  },
};
