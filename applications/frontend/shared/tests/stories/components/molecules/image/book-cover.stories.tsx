import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { BookCover } from "@shared/components/molecules/image/book-cover";

const meta = {
  component: BookCover,
  decorators: [
    (Story) => (
      <div style={{ width: "200px" }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof BookCover>;

export default meta;

export const Default: StoryObj<typeof BookCover> = {
  args: {
    src: "/logo/header-logo.png",
    alt: "サンプル書籍カバー",
    createdAt: new Date("2024-01-15"),
  },
};

export const RecentDate: StoryObj<typeof BookCover> = {
  args: {
    src: "/logo/header-logo.png",
    alt: "最新の書籍カバー",
    createdAt: new Date(),
  },
};
