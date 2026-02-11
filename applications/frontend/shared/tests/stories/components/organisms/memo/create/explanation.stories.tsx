import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { CreateMemoExplanation } from "@shared/components/organisms/memo/create/explanation";

const meta = {
  component: CreateMemoExplanation,
} satisfies Meta<typeof CreateMemoExplanation>;

export default meta;

export const Default: StoryObj<typeof CreateMemoExplanation> = {};
