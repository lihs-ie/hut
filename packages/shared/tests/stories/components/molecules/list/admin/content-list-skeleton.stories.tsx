import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminContentListSkeleton } from "../../../../../../../admin/src/app/admin/_components/molecules/list/content.skeleton";

const meta = {
  component: AdminContentListSkeleton,
  parameters: {
    layout: "padded",
  },
} satisfies Meta<typeof AdminContentListSkeleton>;

export default meta;

type Story = StoryObj<typeof AdminContentListSkeleton>;

export const Default: Story = {
  args: {},
};

export const ThreeItems: Story = {
  args: {
    count: 3,
  },
};

export const TenItems: Story = {
  args: {
    count: 10,
  },
};

export const SingleItem: Story = {
  args: {
    count: 1,
  },
};
