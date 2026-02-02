import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { PlayIcon } from "../../../../../../../admin/src/app/admin/_components/atoms/icon/play";

const meta = {
  component: PlayIcon,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof PlayIcon>;

export default meta;

type Story = StoryObj<typeof PlayIcon>;

export const Default: Story = {
  args: {},
};

export const WithClassName: Story = {
  args: {
    className: "icon-lg",
  },
};
