import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MemoCreateForm } from "@shared/components/organisms/memo/create/form";

const meta = {
  component: MemoCreateForm,
} satisfies Meta<typeof MemoCreateForm>;

export default meta;

export const Default: StoryObj<typeof MemoCreateForm> = {
  args: {
    onCreate: async (title, status) => {
      console.log("Creating memo with title:", title, "and status:", status);
    },
  },
};
