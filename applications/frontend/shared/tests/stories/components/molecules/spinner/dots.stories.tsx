import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { DotsSpinner } from "@shared/components/molecules/spinner/dots";

const meta = {
  component: DotsSpinner,
} satisfies Meta<typeof DotsSpinner>;

export default meta;

export const Default: StoryObj<typeof DotsSpinner> = {};
