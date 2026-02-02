import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { TagCard } from "@shared/components/molecules/list/card/admin/tag";
import { TagMold } from "../../../../../../support/molds/domains/attributes/tag";
import { Forger } from "@lihs-ie/forger-ts";

const meta = {
  component: TagCard,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof TagCard>;
export default meta;

type Story = StoryObj<typeof TagCard>;

const tagForger = Forger(TagMold);

export const Default: Story = {
  args: (() => {
    const tag = tagForger.forge();
    return {
      logo: tag.logo,
      name: tag.name,
      createdAt: tag.timeline.createdAt,
      href: "/tags/edit/123",
    };
  })(),
};

export const LongName: Story = {
  args: (() => {
    const tag = tagForger.forge();
    return {
      logo: tag.logo,
      name: "TypeScript React",
      createdAt: tag.timeline.createdAt,
      href: "/tags/edit/123",
    };
  })(),
};

export const ShortName: Story = {
  args: (() => {
    const tag = tagForger.forge();
    return {
      logo: tag.logo,
      name: "Go",
      createdAt: tag.timeline.createdAt,
      href: "/tags/edit/123",
    };
  })(),
};
