import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SimpleButton } from "@shared/components/atoms/button/simple";

const meta = {
  component: SimpleButton,
} satisfies Meta<typeof SimpleButton>;

export default meta;

export const Default: StoryObj<typeof SimpleButton> = {
  args: {
    children: "Sample",
  },
};
