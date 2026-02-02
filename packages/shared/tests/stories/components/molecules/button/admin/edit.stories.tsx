import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { EditButton } from "../../../../../../../admin/src/app/admin/_components/molecules/button/edit";

const meta = {
  component: EditButton,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof EditButton>;

export default meta;

type Story = StoryObj<typeof EditButton>;

export const Default: Story = {
  args: {
    href: "/admin/articles/1/edit",
  },
};

export const MemoEdit: Story = {
  args: {
    href: "/admin/memos/abc123/edit",
  },
};

export const SeriesEdit: Story = {
  args: {
    href: "/admin/series/xyz789/edit",
  },
};
