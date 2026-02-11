import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { VerticalLine } from "@shared/components/atoms/line/vertical";

const meta = {
  component: VerticalLine,
} satisfies Meta<typeof VerticalLine>;

export default meta;

export const Default: StoryObj<typeof VerticalLine> = {};
