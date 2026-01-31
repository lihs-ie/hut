import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TagBadge } from "@shared/components/molecules/badge/tag";
import { Forger } from "@lihs-ie/forger-ts";
import { TagMold } from "../../../../support/molds/domains/attributes/tag";

const meta = {
  component: TagBadge,
} satisfies Meta<typeof TagBadge>;

export default meta;

const tag = Forger(TagMold).forge();

export const Default: StoryObj<typeof TagBadge> = {
  args: {
    name: tag.name,
    logo: tag.logo,
  },
};
