import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { SearchIcon } from "@shared/components/atoms/icon/search";

const meta = {
  component: SearchIcon,
} satisfies Meta<typeof SearchIcon>;

export default meta;

export const Default: StoryObj<typeof SearchIcon> = {};
