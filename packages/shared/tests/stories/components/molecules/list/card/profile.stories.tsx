import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ProfileCard } from "@shared/components/molecules/list/card/profile";
import { ProfileMold } from "../../../../../support/molds/domains/user";
import { Forger } from "@lihs-ie/forger-ts";

const meta = {
  component: ProfileCard,
} satisfies Meta<typeof ProfileCard>;

export default meta;

export const Default: StoryObj<typeof ProfileCard> = {
  args: {
    profile: Forger(ProfileMold).forge(),
  },
};
