import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SunIcon } from "@shared/components/atoms/icon/sun";

const meta = {
  component: SunIcon,
} satisfies Meta<typeof SunIcon>;

export default meta;

export const Default: StoryObj<typeof SunIcon> = {};
