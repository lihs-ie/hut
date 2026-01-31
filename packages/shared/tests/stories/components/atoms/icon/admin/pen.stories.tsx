import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { PenIcon } from "../../../../../../../admin/src/app/admin/_components/atoms/icon/pen";

const meta = {
  component: PenIcon,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof PenIcon>;

export default meta;

type Story = StoryObj<typeof PenIcon>;

export const Default: Story = {
  args: {},
};

export const WithClassName: Story = {
  args: {
    className: "icon-lg",
  },
};
