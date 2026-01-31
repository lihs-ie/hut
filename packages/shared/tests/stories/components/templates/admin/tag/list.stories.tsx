import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminTagListTemplate } from "@shared/components/templates/admin/tag";
import { TagMold } from "../../../../../support/molds/domains/attributes/tag";
import { Forger } from "@lihs-ie/forger-ts";

const meta = {
  component: AdminTagListTemplate,
  parameters: {
    layout: "fullscreen",
  },
} satisfies Meta<typeof AdminTagListTemplate>;
export default meta;

type Story = StoryObj<typeof AdminTagListTemplate>;

export const Default: Story = {
  args: {
    tags: Forger(TagMold).forgeMulti(5),
    baseHref: "/tags",
    newTagHref: "/tags/new",
  },
};

export const Empty: Story = {
  args: {
    tags: [],
    baseHref: "/tags",
    newTagHref: "/tags/new",
  },
};

export const ManyTags: Story = {
  args: {
    tags: Forger(TagMold).forgeMulti(15),
    baseHref: "/tags",
    newTagHref: "/tags/new",
  },
};
