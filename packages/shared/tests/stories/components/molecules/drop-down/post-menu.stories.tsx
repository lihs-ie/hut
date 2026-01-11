import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { PostMenuDropDown } from "@shared/components/molecules/drop-down/post-menu";

const meta = {
  component: PostMenuDropDown,
  parameters: {
    layout: "centered",
  },
} satisfies Meta<typeof PostMenuDropDown>;
export default meta;

export const Default: StoryObj<typeof PostMenuDropDown> = {};
