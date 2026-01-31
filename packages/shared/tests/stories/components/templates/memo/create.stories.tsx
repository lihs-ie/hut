import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { CreateMemoIndex } from "@shared/components/templates/memo/create";

const meta = {
  component: CreateMemoIndex,
  parameters: {
    nextjs: {
      appDirectory: true,
    },
  },
} satisfies Meta<typeof CreateMemoIndex>;

export default meta;

const persist = async () => {
  console.log("Memo persisted");
};

export const Default: StoryObj<typeof CreateMemoIndex> = {
  args: {
    persist,
  },
};
