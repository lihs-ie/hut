import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { MoonIcon } from "@shared/components/atoms/icon/moon";

const meta = {
  component: MoonIcon,
} satisfies Meta<typeof MoonIcon>;

export default meta;

export const Default: StoryObj<typeof MoonIcon> = {};
