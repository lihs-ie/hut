import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MessageIcon } from "@shared/components/atoms/icon/message";

const meta = {
  component: MessageIcon,
} satisfies Meta<typeof MessageIcon>;

export default meta;

export const Default: StoryObj<typeof MessageIcon> = {};
