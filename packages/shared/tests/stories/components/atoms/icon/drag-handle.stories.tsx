import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { DragHandleIcon } from "@shared/components/atoms/icon/drag-handle";

const meta = {
  component: DragHandleIcon,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof DragHandleIcon>;

export default meta;

type Story = StoryObj<typeof DragHandleIcon>;

export const Default: Story = {
  args: {},
};
