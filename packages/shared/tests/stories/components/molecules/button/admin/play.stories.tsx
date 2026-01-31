import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { PlayButton } from "../../../../../../../admin/src/app/admin/_components/molecules/button/play";

const meta = {
  component: PlayButton,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof PlayButton>;

export default meta;

type Story = StoryObj<typeof PlayButton>;

export const Default: Story = {
  args: {
    href: "/articles/preview/1",
  },
};

export const MemoPreview: Story = {
  args: {
    href: "/memos/preview/abc123",
  },
};

export const SeriesPreview: Story = {
  args: {
    href: "/series/preview/xyz789",
  },
};
