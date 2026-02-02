import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MemoCreateForm } from "@shared/components/organisms/memo/create/form";

const meta = {
  component: MemoCreateForm,
  parameters: {
    nextjs: { appDirectory: true },
  },
} satisfies Meta<typeof MemoCreateForm>;

export default meta;

export const Default: StoryObj<typeof MemoCreateForm> = {
  args: {
    persist: async (unvalidated) => {
      console.log("Persisting memo:", unvalidated);
    },
  },
};
