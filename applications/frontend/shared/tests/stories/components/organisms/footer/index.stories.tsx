import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Footer } from "@shared/components/organisms/footer";
import { Forger } from "@lihs-ie/forger-ts";
import { ProfileMold } from "../../../../support/molds/domains/user";

const meta = {
  component: Footer,
} satisfies Meta<typeof Footer>;

export default meta;

export const Default: StoryObj<typeof Footer> = {
  args: {
    getProfile: async () => Forger(ProfileMold).forge(),
  },
};
