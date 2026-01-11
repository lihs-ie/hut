import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AboutIndex } from "@shared/components/templates/about";
import { Builder } from "../../../../support/molds";
import { ProfileFactory } from "../../../../support/molds/domains/user";
import { profile } from "@shared/config/presentation/profile";
import { TechnologyCategory } from "@shared/domains/common/tech";
import { TechnologyStackFactory } from "../../../../support/molds/domains/common/tech";

const meta = {
  component: AboutIndex,
} satisfies Meta<typeof AboutIndex>;

export default meta;

const getProfile = async () =>
  Builder(ProfileFactory).build({
    techStacks: new Map([
      [
        TechnologyCategory.FRONTEND,
        Builder(TechnologyStackFactory).buildList(3).toArray(),
      ],
      [
        TechnologyCategory.BACKEND,
        Builder(TechnologyStackFactory).buildList(3).toArray(),
      ],
    ]),
  });

export const Default: StoryObj<typeof AboutIndex> = {
  args: {
    getProfile,
    now: new Date("2024-01-01T00:00:00.000Z"),
    logoSources: profile.techStack.logoSources,
  },
};
