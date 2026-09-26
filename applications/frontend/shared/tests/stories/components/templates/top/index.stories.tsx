import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TopIndex } from "@shared/components/templates/top";
import { Forger } from "@lihs-ie/forger-ts";
import { ProfileMold } from "../../../../support/molds/domains/user";

const meta = {
  component: TopIndex,
} satisfies Meta<typeof TopIndex>;

export default meta;

const searchArticles = async () => [];
const getProfile = async () => Forger(ProfileMold).forge();

export const Default: StoryObj<typeof TopIndex> = {
  args: {
    searchArticles,
    findAllTags: async () => [],
    getProfile,
  },
};
