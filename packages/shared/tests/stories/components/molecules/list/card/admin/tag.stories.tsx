import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminTagCard } from "@shared/components/molecules/list/card/admin/tag";
import { Builder } from "../../../../../../support/molds";
import { TagAttributeFactory } from "../../../../../../support/molds/domains/attributes/tag";

const meta = {
  component: AdminTagCard,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof AdminTagCard>;
export default meta;

type Story = StoryObj<typeof AdminTagCard>;

export const Default: Story = {
  args: {
    tag: Builder(TagAttributeFactory).build(),
    href: "/tags/edit/123",
  },
};

export const LongName: Story = {
  args: {
    tag: Builder(TagAttributeFactory).build({ name: "TypeScript React" }),
    href: "/tags/edit/123",
  },
};

export const ShortName: Story = {
  args: {
    tag: Builder(TagAttributeFactory).build({ name: "Go" }),
    href: "/tags/edit/123",
  },
};
