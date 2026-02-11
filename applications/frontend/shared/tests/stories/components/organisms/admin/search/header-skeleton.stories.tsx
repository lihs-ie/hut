import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { AdminSearchHeaderSkeleton } from "../../../../../../../admin/src/app/admin/_components/organisms/search/header.skeleton";

const meta = {
  component: AdminSearchHeaderSkeleton,
  parameters: {
    layout: "padded",
  },
} satisfies Meta<typeof AdminSearchHeaderSkeleton>;

export default meta;

type Story = StoryObj<typeof AdminSearchHeaderSkeleton>;

export const Default: Story = {
  args: {},
};
