import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { Footer } from "@shared/components/organisms/footer";

const meta = {
  component: Footer,
} satisfies Meta<typeof Footer>;

export default meta;

export const Default: StoryObj<typeof Footer> = {};
