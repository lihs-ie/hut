import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AboutIndex } from "@shared/components/templates/about";
import { Forger } from "@lihs-ie/forger-ts";
import { ProfileMold } from "../../../../support/molds/domains/user";
import { TechnologyCategory } from "@shared/domains/common/tech";
import { TechnologyStackMold } from "../../../../support/molds/domains/common/tech";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";

const meta = {
  component: AboutIndex,
} satisfies Meta<typeof AboutIndex>;

export default meta;

// Generate tags first, then use their identifiers for tech stacks
const tags = Forger(TagMold).forgeMultiWithSeed(6, 1);

const frontendTechStacks = tags.slice(0, 3).map((tag, index) =>
  Forger(TechnologyStackMold).forgeWithSeed(index, {
    tag: tag.identifier,
  })
);

const backendTechStacks = tags.slice(3, 6).map((tag, index) =>
  Forger(TechnologyStackMold).forgeWithSeed(index + 3, {
    tag: tag.identifier,
  })
);

const getProfile = async () =>
  Forger(ProfileMold).forge({
    techStacks: new Map([
      [TechnologyCategory.FRONTEND, frontendTechStacks],
      [TechnologyCategory.BACKEND, backendTechStacks],
    ]),
  });

const getAllTags = async () => tags;

export const Default: StoryObj<typeof AboutIndex> = {
  args: {
    getProfile,
    getAllTags,
    now: new Date("2024-01-01T00:00:00.000Z"),
  },
};
