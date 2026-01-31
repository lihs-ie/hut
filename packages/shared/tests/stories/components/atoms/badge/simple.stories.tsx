import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SimpleBadge } from "@shared/components/atoms/badge/simple";

const meta = {
  component: SimpleBadge,
} satisfies Meta<typeof SimpleBadge>;

export default meta;

export const Default: StoryObj<typeof SimpleBadge> = {
  args: {
    label: "バッジ",
  },
};

export const LongLabel: StoryObj<typeof SimpleBadge> = {
  args: {
    label: "これは長いラベルのバッジです",
  },
};
