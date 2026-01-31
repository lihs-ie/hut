import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SelfIcon } from "@shared/components/molecules/image/self";

const meta = {
  component: SelfIcon,
} satisfies Meta<typeof SelfIcon>;

export default meta;

export const Default: StoryObj<typeof SelfIcon> = {};
