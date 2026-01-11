import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminProfileEditTemplate } from "@shared/components/templates/admin/profile/edit";
import { Forger } from "@lihs-ie/forger-ts";
import { ProfileMold } from "../../../../../support/molds/domains/user";
import { Profile, validateProfile } from "@shared/domains/user";
import { unwrapForNextJs } from "@shared/components/global/next-error";

const meta = {
  component: AdminProfileEditTemplate,
  parameters: {
    layout: "fullscreen",
  },
} satisfies Meta<typeof AdminProfileEditTemplate>;

export default meta;

type Story = StoryObj<typeof AdminProfileEditTemplate>;

let persisted: Profile = Forger(ProfileMold).forge();

const getProfile = async () => persisted;

export const Default: Story = {
  args: {
    getProfile,
    persist: async (unvalidated) => {
      await unwrapForNextJs(
        validateProfile(unvalidated)
          .toAsync()
          .map((profile) => {
            persisted = profile;
          }),
      );
    },
  },
};
