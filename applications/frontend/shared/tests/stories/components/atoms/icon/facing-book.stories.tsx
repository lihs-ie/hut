import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { FacingBookIcon } from "@shared/components/atoms/icon/facing-book";

const meta = {
  component: FacingBookIcon,
} satisfies Meta<typeof FacingBookIcon>;

export default meta;

export const Default: StoryObj<typeof FacingBookIcon> = {};
