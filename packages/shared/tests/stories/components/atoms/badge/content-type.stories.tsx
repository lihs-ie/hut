import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { ContentTypeBadge } from "@shared/components/atoms/badge/content-type";

const meta = {
  component: ContentTypeBadge,
} satisfies Meta<typeof ContentTypeBadge>;

export default meta;

export const Default: StoryObj<typeof ContentTypeBadge> = {
  args: {
    variant: "default",
    children: "記事",
  },
};
