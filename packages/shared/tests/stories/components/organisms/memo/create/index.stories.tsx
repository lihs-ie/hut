import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MemoCreate } from "@shared/components/organisms/memo/create";

const meta = {
  component: MemoCreate,
  parameters: {
    nextjs: { appDirectory: true },
  },
} satisfies Meta<typeof MemoCreate>;

export default meta;

export const Default: StoryObj<typeof MemoCreate> = {
  args: {
    persist: async (unvalidated) => {
      console.log("Persisting memo:", unvalidated);
    },
  },
};
