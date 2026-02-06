import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { EraserButton } from "@/app/admin/_components/molecules/button/eraser";

const meta = {
  component: EraserButton,
} satisfies Meta<typeof EraserButton>;
export default meta;

export const Default: StoryObj<typeof EraserButton> = {
  args: {},
};
