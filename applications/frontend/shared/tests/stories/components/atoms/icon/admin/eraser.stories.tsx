import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { EraserIcon } from "../../../../../../../admin/src/app/admin/_components/atoms/icon/eraser";

const meta = {
  component: EraserIcon,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof EraserIcon>;

export default meta;

type Story = StoryObj<typeof EraserIcon>;

export const Default: Story = {
  args: {},
};

export const WithClassName: Story = {
  args: {
    className: "icon-lg",
  },
};
