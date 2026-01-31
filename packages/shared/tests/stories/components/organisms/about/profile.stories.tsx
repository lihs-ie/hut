import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ProfilePresenter } from "@shared/components/organisms/about/profile.presenter";
import { ProfileMold } from "../../../../support/molds/domains/user";
import { Forger } from "@lihs-ie/forger-ts";

const meta = {
  component: ProfilePresenter,
} satisfies Meta<typeof ProfilePresenter>;

export default meta;

export const Default: StoryObj<typeof ProfilePresenter> = {
  args: {
    profile: Forger(ProfileMold).forge(),
  },
};
