import type { Meta, StoryObj } from "@storybook/nextjs-vite";

import { NavigationBar } from "@shared/components/molecules/navigation/bar";

const meta = {
  component: NavigationBar,
} satisfies Meta<typeof NavigationBar>;

export default meta;

export const Default: StoryObj<typeof NavigationBar> = {};
